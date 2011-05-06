#lang racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt")

(require compiler/zo-parse
         racket/match
         racket/list)

(provide parse-bytecode)



(define (parse-bytecode in)
  (let ([compilation-top (zo-parse in)])
    (parse-top compilation-top)))


(define (parse-top a-top)
  (match a-top
    [(struct compilation-top (max-let-depth prefix code))
     (make-Top (parse-prefix prefix) (parse-top-code code))]))


(define (parse-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (make-Prefix 
      (append (map parse-prefix-toplevel toplevels)
              (if (empty? stxs)
                  empty
                  empty ;; fixme
                  )
              (build-list num-lifts
                          (lambda (i)))))]))


;; parse-top-code: (U form Any -> Expression)
(define (parse-top-code code)
  (cond
    [(form? code)
     (parse-form code)]
    [else
     (make-Constant code)]))


;; parse-prefix-toplevel: (U #f symbol global-bucket module-variable) -> (U False Symbol GlobalBucket ModuleVariable)
(define (parse-prefix-toplevel a-toplevel)
  (cond
    [(eq? a-toplevel #f)
     #f]
    [(symbol? a-toplevel)
     a-toplevel]
    [(global-bucket? a-toplevel)
     