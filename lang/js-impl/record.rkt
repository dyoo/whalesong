#lang racket/base

(provide record-implementations! 
         record-exports!
         lookup-implementations
         lookup-exports)

(define-struct record (path impls))
(define-struct export (path exports))

(define records '())
(define exports '())

;; record!: path (listof string) -> void
(define (record-implementations! a-path impls)
  (set! records (cons (make-record a-path impls)
                      records)))


;; record-exports!: path (listof symbol) -> void
(define (record-exports! a-path export-names)
  (set! exports (cons (make-export a-path export-names)
                      exports)))


(define (my-foldl f acc lst)
  (cond
    [(null? lst)
     acc]
    [else
     (my-foldl f (f (car lst) acc) (cdr lst))]))
                    

(define (my-filter f lst)
  (cond
    [(null? lst)
     '()]
    [(f (car lst))
     (cons (car lst) (my-filter f (cdr lst)))]
    [else
     (my-filter f (cdr lst))]))


;; lookup-implementations: path -> (listof string)
(define (lookup-implementations a-path)
  (my-foldl (lambda (a-record perms)
           (append (record-impls a-record) perms))
         '()
         (my-filter (lambda (a-record)
                   (equal? a-path (record-path a-record)))
                 records)))


;; lookup-exports: path -> (listof symbol)
(define (lookup-exports a-path)
  (my-foldl (lambda (an-export exports)
           (append (export-exports an-export) exports))
         '()
         (my-filter (lambda (an-export)
                   (equal? a-path (export-path an-export)))
                 exports)))