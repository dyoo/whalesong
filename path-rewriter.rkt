#lang racket/base

(require racket/path
         racket/contract
         racket/list)



(provide/contract [rewrite-path (complete-path? . -> . (or/c symbol? false/c))]
                  [current-root-path parameter?])





(define current-root-path
  (make-parameter (current-directory)))


;; The path rewriter takes paths and provides a canonical symbol for it.
;; Paths located within collects get remapped to collects/....


;; rewrite-path: path -> (symbol #f)
(define (rewrite-path a-path)
  (let ([a-path (normalize-path a-path)])
    (cond
     [(within-collects? a-path)
      (string->symbol
       (string-append "collects/"
                      (path->string
                       (find-relative-path collects a-path))))]
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