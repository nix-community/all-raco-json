#lang racket/base

(require json)
(require racket/function)
(require racket/match)
(require racket/sequence)
(require racket/set)
(require racket/stream)

(provide to-json)

(define (to-json input-path output-path)
  (with-output-to-file output-path
    (lambda ()
      (let* ([inp0 (with-input-from-file input-path read)]
             [inp1 (reshape-dependencies inp0)]
             [inp2 (string-keys-to-symbol-keys inp1)]
             [inp3 (symbol-values-to-string-values inp2)])
        (write-json inp3)))
    #:exists 'replace))

(define (reshape-dependencies ht)
  (hash-map/copy ht
                 (lambda (pkg-name pkg-hash-table)
                   (values pkg-name
                           (hash-update pkg-hash-table
                                        'dependencies
                                        (lambda (dependencies)
                                          (make-immutable-hash (map (lambda (dep)
                                                                      (if (string? dep)
                                                                          `(,dep . "default")
                                                                          (let ((dep-name (car dep))
                                                                                (version (if (memq '#:version dep)
                                                                                             (cadr (memq '#:version dep))
                                                                                             "default")))
                                                                            `(,dep-name . ,version))))
                                                                    dependencies))))))))

(define (string-keys-to-symbol-keys expr)
  (cond
    [(hash? expr) (hash-map/copy expr (lambda (k v)
                                        (values (if (string? k)
                                                    (string->symbol k)
                                                    k)
                                                (string-keys-to-symbol-keys v))))]
    [(list? expr) (map string-keys-to-symbol-keys expr)]
    [else expr]))

(define (symbol-values-to-string-values value)
  (cond
    [(symbol? value) (symbol->string value)]
    [(hash? value) (hash-map/copy value (lambda (k v)
                                          (values k
                                                  (symbol-values-to-string-values v))))]
    [(list? value) (map symbol-values-to-string-values value)]
    [else value]))

;; Return a copy of the input hash table that
;; 1. does not have any packages tagged "main-distribution" or "main-test"
;; 2. ensures that any such packages are also removed from the dependency list of all remaining packages
(define (keep-only-relevant-deps ht)
  (let* ([bundled-tags '("main-distribution" "main-test")]
         [all-package-names (apply set (hash-keys ht))]
         [bundled-packages (set-add (apply set (stream->list
                                                (sequence-filter
                                                 (lambda (pkg-name)
                                                   (ormap (lambda (tag)
                                                            ;; XXX: would prefer to use memq, but tag is mutable for some reason
                                                            (member tag bundled-tags))
                                                          (hash-ref (hash-ref ht pkg-name) 'tags)))
                                                 all-package-names)))
                                    ;; Some people add racket itself as a dependency for some reason
                                    "racket")]
         [relevant-packages (set->list (set-subtract all-package-names
                                                     bundled-packages))])
    (make-immutable-hash (map (lambda (pkg-name)
                                (let* ([pkg-hash-table (hash-ref ht pkg-name)]
                                       [pkg-deps (hash->list (hash-ref pkg-hash-table 'dependencies))]
                                       [pkg-kept-deps (filter (match-lambda
                                                                [(cons pkg-name _)
                                                                 (not (set-member? bundled-packages pkg-name))])
                                                              pkg-deps)]
                                       [final-pkg-deps (make-immutable-hash pkg-kept-deps)])
                                  (cons pkg-name
                                        (hash-set pkg-hash-table 'dependencies final-pkg-deps))))
                              relevant-packages))))

;; Assume irrelevant deps have been removed
;; Construct a graph in which an edge v -> u denotes that u depends on v
(define (make-reverse-graph ht)
  (sequence-fold (match-lambda**
                  [(accum pkg-name (hash-table ('dependencies dependencies)))
                   (foldl (lambda (dep-name prev-accum)
                            (hash-update prev-accum
                                         dep-name
                                         (curry cons pkg-name)
                                         ;; Dependency names can be
                                         ;; URLs, or some weird name
                                         ;; containing architecture/OS
                                         ;; information.  Such names
                                         ;; will not necessarily be in
                                         ;; top level of the catalog
                                         ;; at all, so we initialize a
                                         ;; default value here.
                                         '()))
                          accum
                          (map car (hash->list dependencies)))])
                 (make-immutable-hash (map (lambda (pkg-name)
                                             `(,pkg-name . ()))
                                           (hash-keys ht)))
                 ht))

(define DUMMY-PKG "nix-dummy-pkg")

(define (make-reverse-graph-with-dummy-node ht)
  (let* ([graph (make-reverse-graph ht)]
         ;; Draw edges from the dummy package to any package in the
         ;; top-level hash table whose source_url doesn't correspond
         ;; to a git repo
         [non-git-top-level-pkg-names (filter (lambda (pkg-name)
                                                ;; This should work unless the source_url looks like
                                                ;; https://some-git-repo.com/my-project.git?path=weird.zip
                                                ;; where weird.zip is a directory
                                                ;; or
                                                ;; "http://github.com/some-user/some-racket-repo/tree/weird.zip"
                                                ;; where weird.zip is a branch
                                                (regexp-match #rx"\\.(?:zip|tgz|(?:tar)?\\.gz|plt)$"
                                                              (hash-ref
                                                               (hash-ref
                                                                (hash-ref
                                                                 (hash-ref ht pkg-name)
                                                                 'versions) 'default) 'source_url)))
                                              ;; Here we use ht
                                              ;; instead of the graph
                                              ;; keys because the
                                              ;; graph may have keys
                                              ;; corresponding to
                                              ;; packages not in the
                                              ;; top-level hash table
                                              (hash-keys ht))]
         ;; Draw edges from the dummy package to packages containing
         ;; dependencies like "https://github.com/some-user/some-repo" or "tangerine-x86_64-linux"
         [pkgs-not-in-catalog (filter (lambda (pkg-name)
                                        (not (hash-has-key? ht pkg-name)))
                                      (hash-keys graph))]
         [immediately-inadmissible-pkgs (set->list (apply set (append non-git-top-level-pkg-names
                                                                      pkgs-not-in-catalog)))])
    (hash-set graph
              DUMMY-PKG
              immediately-inadmissible-pkgs)))

(define (keep-only-admissible-pkgs ht)
  (letrec ([reverse-graph-with-dummy-node (make-reverse-graph-with-dummy-node ht)]
           [dfs (lambda (u seen-nodes)
                  (foldl (lambda (v prev-seen-nodes)
                           (if (set-member? prev-seen-nodes v)
                               prev-seen-nodes
                               (dfs v (set-add prev-seen-nodes v))))
                         seen-nodes
                         (hash-ref reverse-graph-with-dummy-node u)))]
           [inadmissible-packages (dfs DUMMY-PKG (set DUMMY-PKG))]
           [admissible-packages (set-subtract (list->set (hash-keys reverse-graph-with-dummy-node))
                                              inadmissible-packages)])
    (make-immutable-hash (set-map admissible-packages
                                  (lambda (pkg-name)
                                    (cons pkg-name (hash-ref ht pkg-name)))))))

(define (test-keep-only-admissible-pkgs)
  (let* ([ht (with-input-from-file "pkgs-all" read)]
         [relevant-packages-ht (keep-only-relevant-deps (reshape-dependencies ht))]
         [admissible-packages (keep-only-admissible-pkgs relevant-packages-ht)])
    (sequence-fold (match-lambda** [(acc _pkg-name (hash-table ('dependencies dependencies)))
                                    (sequence-fold (lambda (accum dep-name _)
                                                     (and accum
                                                          (hash-has-key? admissible-packages dep-name)))
                                                   acc
                                                   dependencies)])
                   #t
                   admissible-packages)))
