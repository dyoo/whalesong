#lang racket/base

(provide record-resource
         get-records)


;; Needs to be prefabricated
(struct resource (path key content) #:prefab)


(define records (make-hash))

(define (get-records a-path)
  (hash-ref records a-path '()))


;; Hack to work around bug that should be fixed after 5.1.3.  The dynamic
;; require-for-syntax stuff isn't quite working right, which means
;; we can't use (require racket/port) here.
(define (port->bytes p)
  (define out (open-output-bytes))
  (let loop ()
    (define b (read-byte p))
    (cond
     [(eof-object? b)
      (get-output-bytes out)]
     [else
      (write-byte b out)
      (loop)])))


;; record-javascript-implementation!: path path a-resource-path -> void
(define (record-resource a-module-path a-resource-path a-key)
  (hash-set! records a-module-path
             (cons (resource a-resource-path a-key (call-with-input-file a-resource-path
                                                     port->bytes))
                   (hash-ref records a-module-path '()))))
