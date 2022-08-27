#lang racket/base

(require json)

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
