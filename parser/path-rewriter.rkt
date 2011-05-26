#lang racket/base

(require "../parameters.rkt"
         "where-is-collects.rkt"
         racket/path
         racket/contract
         racket/list
         racket/runtime-path)



(provide/contract [rewrite-path (complete-path? . -> . (or/c symbol? false/c))])



(define-runtime-path whalesong-path "..")
(define normal-whalesong-path
  (let ()
    (normalize-path whalesong-path)))




;; The path rewriter takes paths and provides a canonical symbol for
;; it.  Paths located within collects get remapped to collects, those
;; within the compiler directory mapped to "whalesong", those within
;; the root to "root".  If none of these work, we return #f.


;; rewrite-path: path -> (symbol #f)
(define (rewrite-path a-path)
  (let ([a-path (normalize-path a-path)])
    (cond
     [(within-this-project-path? a-path)
      (string->symbol
       (string-append "whalesong/"
                      (path->string
                       (find-relative-path normal-whalesong-path a-path))))]
     [(within-collects? a-path)
      (string->symbol
       (string-append "collects/"
                      (path->string
                       (find-relative-path collects-path a-path))))]
     [(within-root? a-path)
      (string->symbol
       (string-append "root/"
                      (path->string
                       (find-relative-path (current-root-path) a-path))))]
     [else 
      #f])))


       

(define (within-root? a-path)
  (within? (current-root-path) a-path))


(define (within-collects? a-path)
  (within? collects-path a-path))


(define (within-this-project-path? a-path)
  (within? normal-whalesong-path a-path))


;; within?: normalized-path normalized-path -> boolean
;; Produces true if a-path is within the base.
(define (within? base a-path)
  (let ([rp (find-relative-path base a-path)])
    (cond
     [(equal? rp a-path)
      #f]
     [else
      (let ([chunks (explode-path rp)])
        (cond
         [(empty? chunks)
          #t]
         [(eq? (first chunks) 'up)
          #f]
         [else
          #t]))])))