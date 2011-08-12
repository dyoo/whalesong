#lang racket/base
(require racket/port)
(provide record-resource)


(define-struct record (key resource-path bytes))
(define records '())


;; record-javascript-implementation!: path a-resource-path -> void
(define (record-resource a-key a-resource-path)
  (set! records (cons (make-record a-key 
                                   a-resource-path 
                                   (call-with-input-file a-resource-path port->bytes))
                      records)))
