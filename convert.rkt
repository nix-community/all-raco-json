#lang racket/base

(require json)

(provide to-json)

(define (to-json input-path output-path)
  (with-output-to-file output-path
    (lambda ()
      (let* ((inp0 (with-input-from-file input-path read))
             (inp1 (convert-immutable-to-mutable-hash-table inp0))
             (inp2 (reshape-dependencies inp1))
             (inp3 (string-keys-to-symbol-keys inp2))
             (inp4 (symbol-values-to-string-values inp3)))
        (write-json inp4)))
    #:exists 'replace))

(define (convert-immutable-to-mutable-hash-table value)
  (cond
    [(hash? value) (hash-map/copy value
                                  (lambda (k v)
                                    (values k
                                            (convert-immutable-to-mutable-hash-table v)))
                                  #:kind 'mutable)]
    [(list? value) (map convert-immutable-to-mutable-hash-table value)]
    [else value]))

(define (reshape-dependencies ht)
  (hash-for-each ht
                 (lambda (pkg-name pkg-hash-table)
                   (hash-update! pkg-hash-table
                                 'dependencies
                                 (lambda (dependencies)
                                   (make-hash (map (lambda (dep)
                                                     (if (string? dep)
                                                         `(,dep . "default")
                                                         (let ((dep-name (car dep))
                                                               (version (if (memq '#:version dep)
                                                                            (cadr (memq '#:version dep))
                                                                            "default")))
                                                           `(,dep-name . ,version))))
                                                   dependencies))))))
  ht)

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
