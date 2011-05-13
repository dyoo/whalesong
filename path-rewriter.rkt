#lang racket/base

(require "parameters.rkt"
         racket/path
         racket/contract
         racket/list
         racket/runtime-path)



(provide/contract [rewrite-path (complete-path? . -> . (or/c symbol? false/c))])



(define-runtime-path this-path ".")
(define this-normal-path
  (let ()
    (normalize-path this-path)))




;; The path rewriter takes paths and provides a canonical symbol for it.
;; Paths located within collects get remapped to collects, those within
;; the compiler directory mapped to "js-vm", those within the root to "root".
;; If none of these work, we return #f.

;; rewrite-path: path -> (symbol #f)
(define (rewrite-path a-path)
  (let ([a-path (normalize-path a-path)])
    (cond
     [(within-collects? a-path)
      (string->symbol
       (string-append "collects/"
                      (path->string
                       (find-relative-path collects a-path))))]
     [(within-this-project-path? a-path)
      (string->symbol
       (string-append "js-vm/"
                      (path->string
                       (find-relative-path this-normal-path a-path))))]
     [(within-root? a-path)
      (string->symbol
       (string-append "root/"
                      (path->string
                       (find-relative-path (current-root-path) a-path))))]
     [else 
      #f])))


(define collects
  (normalize-path
   (let ([p (find-system-path 'collects-dir)])
     (cond
      [(relative-path? p)
       (find-executable-path (find-system-path 'exec-file)
                             (find-system-path 'collects-dir))]
      [else
       p]))))
        


(define (within-root? a-path)
  (within? (current-root-path) a-path))


(define (within-collects? a-path)
  (within? collects a-path))


(define (within-this-project-path? a-path)
  (within? this-normal-path a-path))


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