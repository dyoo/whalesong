#lang racket/base
(require racket/port)

(provide record-resource
         get-records)


;; Needs to be prefabricated
(struct resource (path key content) #:prefab)


(define records (make-hash))

(define (get-records a-path)
  (hash-ref records a-path '()))


;; record-javascript-implementation!: path path a-resource-path -> void
(define (record-resource a-module-path a-resource-path a-key)
  (hash-set! records a-module-path
             (cons (resource a-resource-path a-key (call-with-input-file a-resource-path
                                                     port->bytes))
                   (hash-ref records a-module-path '()))))
