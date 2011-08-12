#lang racket/base
(require racket/port
         "structs.rkt")
(provide record-resource
         get-records)

(define records '())

(define (get-records)
  records)

;; record-javascript-implementation!: path a-resource-path -> void
(define (record-resource a-resource-path a-key)
  (set! records (cons (resource a-resource-path a-key)
                      records)))
