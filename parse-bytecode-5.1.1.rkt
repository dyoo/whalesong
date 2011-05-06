#lang racket/base

(require "expression-structs.rkt"
         "lexical-structs.rkt")


;; Parsing Racket 5.1.1 bytecode structures into our own.
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
                  (list #f))
              (build-list num-lifts
                          (lambda (i)
                            #f))))]))


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
     (make-GlobalBucket (global-bucket-name a-toplevel))]
    [(module-variable? a-toplevel)
     (make-ModuleVariable 
      ;; fixme: we need to remember more than just the name of the symbol!
      (module-variable-sym a-toplevel))]))


;; parse-form: form -> (U Expression)
(define (parse-form a-form)
  (cond
    [(def-values? a-form)
     (parse-def-values a-form)]

    [(def-syntaxes? a-form)
     ;; Ignore def-syntaxes.
     (parse-def-syntaxes? a-form)]
    
    [(req? a-form)
     (parse-req a-form)]
    
    [(seq? a-form)
     (parse-seq a-form)]
    
    [(splice? a-form)
     (parse-splice a-form)]
    
    [(mod? a-form)
     (parse-mod a-form)]
    
    [(expr? a-form)
     (parse-expr a-form)]

    [else
     (error 'parse-form "~s" a-form)]))


(define (parse-def-values form)
  (error 'fixme))
(define (parse-def-syntaxes form)
  (error 'fixme))
(define (parse-req form)
  (error 'fixme))
(define (parse-seq form)
  (error 'fixme))
(define (parse-splice form)
  (error 'fixme))
(define (parse-mod form)
  (error 'fixme))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-expr expr)
  (cond
    [(lam? expr)
     (parse-lam expr)]
    [(closure? expr)
     (parse-closure expr)]
    [(case-lam? expr)
     (parse-case-lam expr)]
    [(let-one? expr)
     (parse-let-one expr)]
    [(let-void? expr)
     (parse-let-void expr)]
    [(install-value? expr)
     (parse-install-value expr)]
    [(let-rec? expr)
     (parse-let-rec expr)]
    [(boxenv? expr)
     (parse-boxenv expr)]
    [(localref? expr)
     (parse-localref expr)]
    [(toplevel? expr)
     (parse-toplevel expr)]
    [(topsyntax? expr)
     (parse-topsyntax expr)]
    [(application? expr)
     (parse-application expr)]
    [(branch? expr)
     (parse-branch expr)]
    [(with-cont-mark? expr)
     (parse-with-cont-mark expr)]
    [(beg0? expr)
     (parse-beg0 expr)]
    [(varref? expr)
     (parse-varref expr)]
    [(assign? expr)
     (parse-assign expr)]
    [(apply-values? expr)
     (parse-apply-values expr)]
    [(primval? expr)
     (parse-primval expr)]))

(define (parse-lam expr)
  (error 'fixme))
(define (parse-closure expr)
  (error 'fixme))
(define (parse-case-lam exp)
  (error 'fixme))
(define (parse-let-one expr)
  (error 'fixme))
(define (parse-let-void expr)
  (error 'fixme))
(define (parse-install-value expr)
  (error 'fixme))
(define (parse-let-rec expr)
  (error 'fixme))
(define (parse-boxenv expr)
  (error 'fixme))
(define (parse-localref expr)
  (error 'fixme))
(define (parse-toplevel expr)
  (error 'fixme))
(define (parse-topsyntax expr)
  (error 'fixme))
(define (parse-application expr)
  (error 'fixme))
(define (parse-branch expr)
  (error 'fixme))
(define (parse-with-cont-mark expr)
  (error 'fixme))
(define (parse-beg0 expr)
  (error 'fixme))
(define (parse-varref expr)
  (error 'fixme))
(define (parse-assign expr)
  (error 'fixme))
(define (parse-apply-values expr)
  (error 'fixme))
(define (parse-primval expr)
  (error 'fixme))