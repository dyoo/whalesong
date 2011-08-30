#lang s-exp "../lang/base.rkt"

(provide (all-defined-out))

(define-struct event (kvpairs))


(define (event-keys an-evt)
  (map car (event-kvpairs an-evt)))


(define (event-ref an-evt a-key)
  (define clean-key (cond
                     [(symbol? a-key)
                      a-key]
                     [(string? a-key)
                      (string->symbol a-key)]
                     [else
                      (raise-type-error 'event-ref "symbol or string" a-key)]))
  (define kv (assq clean-key (event-kvpairs an-evt)))
  (cond [(eq? kv #f)
         (error 'event-ref "Could not find key ~a" a-key)]
        [else
         (car (cdr kv))]))
      
