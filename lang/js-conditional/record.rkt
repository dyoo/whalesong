#lang racket/base

(provide record-javascript-implementation!
         has-javascript-implementation?
         lookup-javascript-implementation
         
         record-redirection!
         follow-redirection)


(define-struct record (path impl))
(define records '())

(define-struct redirection (from to))
(define redirections '())

;; record-javascript-implementation!: path string -> void
(define (record-javascript-implementation! a-path an-impl)
  (set! records (cons (make-record a-path an-impl)
                      records)))

;; has-javascript-implementation?: path -> boolean
(define (has-javascript-implementation? a-path)
  (let loop ([lst records])
    (cond
      [(null? lst)
       #f]
      [(equal? a-path (record-path (car lst)))
       #t]
      [else
       (loop (cdr lst))])))


;; find: path (listof record) -> record
(define (find path lst)
  (cond
    [(null? lst)
     (error 'find "Couldn't find ~s" path)]
    [(equal? path (record-path (car lst)))
     (car lst)]
    [else
     (find path (cdr lst))]))


;; lookup-javascript-implementation: path -> module-path
(define (lookup-javascript-implementation a-path)
  (record-impl (find a-path records)))


(define (record-redirection! from to)
  (set! redirections (cons (make-redirection from to) redirections)))


(define (follow-redirection a-path)
  (let loop ([redirections redirections])
    (cond
      [(null? redirections)
       #f]
      [(equal? (redirection-from (car redirections)) a-path)
       (redirection-to (car redirections))]
      [else
       (loop (cdr redirections))])))