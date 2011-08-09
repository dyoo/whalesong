#lang racket/base

(require syntax/kerncase
         (for-template (planet dyoo/whalesong/lang/kernel)
                       "resource.rkt")
         "resource.rkt")

(provide expand-out-images)


;; expand-out-images: syntax -> compiled-code
(define (expand-out-images stx)
  (define expanded (expand stx))
  
  (define rewritten
    (kernel-syntax-case (syntax-disarm expanded code-insp) #f
             [(#%expression expr)
              (quasisyntax/loc stx
                (#%expression #,(on-expr #'expr)))]

             [(module id name-id (#%plain-module-begin module-level-form ...))
              #`(module id name-id (#%plain-module-begin 
                                    (require (planet dyoo/whalesong/resource))
                                    #,@(map convert-images-to-resources
                                            (syntax->list #'(module-level-form ...)))))]
             [(begin top-level-form ...)
              (quasisyntax/loc stx
                (begin #,@(map convert-images-to-resources 
                               (syntax->list #'(top-level-form ...)))))]
             [else
              (convert-images-to-resources expanded)]))
  
  ;; We need to translate image snips in the expanded form so we can
  ;; fruitfully use compiler/zo-parse.    
  (compile rewritten))


(define code-insp (current-code-inspector))


(define (on-expr expr)
  (kernel-syntax-case (syntax-disarm expr code-insp) #f
    
    [(#%plain-lambda formals subexpr ...)
     (quasisyntax/loc expr
       (#%plain-lambda formals #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(case-lambda case-lambda-clauses ...)
     (quasisyntax/loc expr
       (case-lambda #,@(map (lambda (a-clause)
                              (syntax-case (syntax-disarm a-clause code-insp) ()
                                [(formals subexpr ...)
                                 (quasisyntax/loc a-clause
                                   (formals #,@(map on-expr #'(subexpr ...))))]))
                            (syntax->list #'(case-lambda-clauses ...)))))]
    
    [(if test true-part false-part)
     (quasisyntax/loc expr
       (if #,(on-expr #'test)
           #,(on-expr #'true-part)
           #,(on-expr #'false-part)))]
    
    [(begin subexpr ...)
     (quasisyntax/loc expr
       (begin #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(begin0 subexpr ...)
     (quasisyntax/loc expr
       (begin0 #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(let-values bindingss body ...)
     (quasisyntax/loc expr
       (let-values #,(syntax-case (syntax-disarm #'bindingss code-insp) ()
                       [(binding ...)
                        (quasisyntax/loc #'bindings
                          (#,@(map (lambda (binding) 
                                     (syntax-case (syntax-disarm binding code-insp) ()
                                       [(ids expr)
                                        (quasisyntax/loc binding
                                          (ids #,(on-expr #'expr)))]))
                                   (syntax->list #'(binding ...)))))])
         #,@(map on-expr (syntax->list #'(body ...)))))]
    
    [(letrec-values bindingss body ...)
     (quasisyntax/loc expr
       (letrec-values #,(syntax-case (syntax-disarm #'bindingss code-insp) ()
                          [(binding ...)
                           (quasisyntax/loc #'bindings
                             (#,@(map (lambda (binding) 
                                        (syntax-case (syntax-disarm binding code-insp) ()
                                          [(ids expr)
                                           (quasisyntax/loc binding
                                             (ids #,(on-expr #'expr)))]))
                                      (syntax->list #'(binding ...)))))])
         #,@(map on-expr (syntax->list #'(body ...)))))]
    
    [(set! id subexpr)
     (quasisyntax/loc expr
       (set! id #,(on-expr #'subexpr)))]
    
    [(quote datum)
     (on-datum #'datum (lambda (v)
                         (quasisyntax/loc expr
                           (quote #,v))))]
    
    [(quote-syntax datum)
     (on-datum #'datum (lambda (v)
                         (quasisyntax/loc expr
                           (quote-syntax #,v))))]
    
    [(with-continuation-mark key value body)
     (quasisyntax/loc expr
       (with-continuation-mark #,(on-expr #'key) #,(on-expr #'value) #,(on-expr #'body)))]
    
    [(#%plain-app subexpr ...)
     (quasisyntax/loc expr
       (#%plain-app #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(#%top . id)
     expr]
    
    [(#%variable-reference (#%top . id))
     expr]
    [(#%variable-reference id)
     expr]
    [(#%variable-reference)
     expr]
    [else 
     expr]))


(define (on-datum datum-stx k)
  (define-values (image? convert) 
    (values
     (dynamic-require '2htdp/image 'image?)
     (dynamic-require 'file/convertible 'convert)))
  (cond
    [(image? (syntax-e datum-stx))
     (with-syntax ([image-bytes (convert (syntax-e datum-stx) 'png-bytes)])
       (quasisyntax/loc datum-stx
         (make-bytes-resource #f #f image-bytes)))]
    
    [else
     (k datum-stx)]))



(define (convert-images-to-resources stx)
  (kernel-syntax-case (syntax-disarm stx code-insp) #f
    [(#%provide raw-provide-spec ...)
     stx]
    
    [(#%require raw-require-spec ...)
     stx]
    
    [(define-values ids expr)
     (quasisyntax/loc stx 
       (define-values ids #,(on-expr #'expr)))]
    
    [(define-syntaxes ids expr)
     (quasisyntax/loc stx 
       (define-syntaxes ids #,(on-expr #'expr)))]
    
    [(define-values-for-syntax ids expr)
     (quasisyntax/loc stx 
       (define-values-for-syntax ids #,(on-expr #'expr)))]
    
    [else
     (on-expr stx)]))