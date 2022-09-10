#lang racket/base

(require racket/file)
(require racket/function)
(require racket/match)
(require racket/sequence)
(require racket/set)
(require racket/stream)

(provide write-catalog
         test-keep-only-admissible-pkgs)

(define main-tags '("main-distribution" "main-tests"))

;; Given a pkgs-all file, remove select nodes and also
;; generate files according to the catalog directory structure
(define (write-catalog pkgs-all-input-path)
  (let* ([original-ht (with-input-from-file pkgs-all-input-path read)]
         [admissible-pkg-set (keep-only-admissible-pkgs (keep-only-relevant-deps (reshape-dependencies original-ht)))]
         [final-ht (reshape-dependencies
                    (make-immutable-hash
                     (set-map admissible-pkg-set
                              (lambda (pkg-name)
                                `(,pkg-name . ,(hash-ref original-ht pkg-name))))))])
    (with-output-to-file "pkgs-all"
      (lambda ()
        (write final-ht))
      #:exists 'replace)

    (with-output-to-file "pkgs"
      (lambda ()
        (write (hash-keys final-ht)))
      #:exists 'replace)

    (when (directory-exists? "pkg/")
      (delete-directory/files "pkg/"))
    (make-directory "pkg/")

    (hash-for-each final-ht
                   (lambda (pkg-name pkg-hash-table)
                     (with-output-to-file (string-append-immutable "pkg/" pkg-name)
                       (lambda ()
                         (write pkg-hash-table)))))))

(define (reshape-dependencies ht)
  (hash-map/copy ht
                 (lambda (pkg-name pkg-hash-table)
                   (values pkg-name
                           (let* ([pkg-hash-table-with-simplified-deps
                                   (hash-update pkg-hash-table
                                                'dependencies
                                                (lambda (dependencies)
                                                  ;; Ignore version since
                                                  ;; 1. presumably upstream nixpkgs keeps Racket reasonably up to date
                                                  ;; 2. Racket's versioning of packages makes no sense
                                                  (map (match-lambda
                                                         [(or (cons dep-name _) dep-name)
                                                          dep-name])
                                                       dependencies)))]
                                  [pkg-hash-table-with-overwritten-source
                                   (hash-set pkg-hash-table-with-simplified-deps
                                             'source
                                             (hash-ref (hash-ref (hash-ref
                                                                  pkg-hash-table-with-simplified-deps
                                                                  'versions) 'default) 'source_url))]

                                  [pkg-hash-table-with-no-old-versions
                                   (hash-update pkg-hash-table-with-overwritten-source
                                                'versions
                                                (match-lambda
                                                  [(hash-table ('default default-ht))
                                                   (make-immutable-hash `((default . ,default-ht)))]))])
                             pkg-hash-table-with-no-old-versions)))))

;; Return a copy of the input hash table that
;; 1. does not have any packages tagged "main-distribution" or "main-test"
;; 2. ensures that any such packages are also removed from the dependency list of all remaining packages
(define (keep-only-relevant-deps ht)
  (let* ([all-package-names (apply set (hash-keys ht))]
         [bundled-packages (apply set
                                  ;; Some people add racket itself as a dependency for some reason
                                  "racket"
                                  (stream->list
                                   (sequence-filter
                                    (lambda (pkg-name)
                                      (ormap (lambda (tag)
                                               ;; XXX: would prefer to use memq, but tag is mutable for some reason
                                               (member tag main-tags))
                                             (hash-ref (hash-ref ht pkg-name) 'tags)))
                                    all-package-names)))]
         [relevant-packages (set-subtract all-package-names bundled-packages)])
    (make-immutable-hash (set-map relevant-packages
                                  (lambda (pkg-name)
                                    (let* ([pkg-hash-table (hash-ref ht pkg-name)]
                                           [pkg-kept-deps (filter (lambda (pkg-name)
                                                                    (not (set-member? bundled-packages pkg-name)))
                                                                  (hash-ref pkg-hash-table 'dependencies))])
                                      `(,pkg-name . ,(hash-set pkg-hash-table 'dependencies pkg-kept-deps))))))))

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
                          dependencies)])
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
                  (if (set-member? seen-nodes u)
                      seen-nodes
                      (foldl dfs
                             (set-add seen-nodes u)
                             (hash-ref reverse-graph-with-dummy-node u))))]
           [inadmissible-packages (dfs DUMMY-PKG (set))])
    (set-subtract (list->set (hash-keys reverse-graph-with-dummy-node))
                  inadmissible-packages)))

(define (test-keep-only-admissible-pkgs)
  (let* ([ht (with-input-from-file "input-pkgs-all" read)]
         [relevant-packages-ht (keep-only-relevant-deps (reshape-dependencies ht))]
         [admissible-packages (keep-only-admissible-pkgs relevant-packages-ht)])
    (sequence-fold (lambda (acc pkg-name)
                     (sequence-fold (match-lambda** [(accum (or (cons dep-name _) dep-name))
                                                     (and accum
                                                          (or
                                                           (string=? dep-name "racket")
                                                           (ormap (lambda (tag)
                                                                    (member tag main-tags))
                                                                  (hash-ref (hash-ref ht dep-name) 'tags))
                                                           (set-member? admissible-packages dep-name)))])
                                    acc
                                    (hash-ref (hash-ref ht pkg-name) 'dependencies)))
                   #t
                   admissible-packages)))
