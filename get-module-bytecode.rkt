#lang racket/base
(require racket/path
         racket/runtime-path
         syntax/modcode
         racket/contract
         "language-namespace.rkt"
         "logger.rkt"
         syntax/kerncase
         (for-template "resource.rkt"))

(provide get-module-bytecode)


(define-runtime-path kernel-language-path
  "lang/kernel.rkt")






(define (get-module-bytecode x)
  (log-debug "grabbing module bytecode for ~s" x)
  (let ([compiled-code
         (cond
           ;; Assumed to be a path string
           [(string? x)
            (get-compiled-code-from-path (normalize-path (build-path x)))]
           
           [(path? x)
            (get-compiled-code-from-path x)]
           
           ;; Input port is assumed to contain the text of a module.
           [(input-port? x)
            (get-compiled-code-from-port x)]
           
           [else
            (error 'get-module-bytecode)])])
    (let ([op (open-output-bytes)])
      (write compiled-code op)
      (get-output-bytes op))))


;; Tries to use get-module-code to grab at module bytecode.  Sometimes
;; this fails because it appears get-module-code tries to write to
;; compiled/.
(define (get-compiled-code-from-path p)
  (with-handlers ([void (lambda (exn)
                          ;; Failsafe: try to do it from scratch
                          (call-with-input-file* p
                            (lambda (ip)
                              (get-compiled-code-from-port ip))))])
    (get-module-code p)))






(define base-namespace
  (lookup-language-namespace
   #;'racket/base
   `(file ,(path->string kernel-language-path)))
  #;(make-base-namespace))


(define (get-compiled-code-from-port ip)
  (parameterize ([read-accept-reader #t]
                 [current-namespace base-namespace])
    (define expanded (expand (read-syntax (object-name ip) ip)))
    ;; We need to translate image snips in the expanded form so we can
    ;; fruitfully use compiler/zo-parse.    
    (compile (kernel-syntax-case expanded #f
               [(module id name-id (#%plain-module-begin module-level-form ...))
                
                #`(module id name-id (#%plain-module-begin 
                                      #,@(map convert-images-to-resources
                                              (syntax->list #'(module-level-form ...)))))]))))


(define code-insp (current-code-inspector))


(define (on-expr expr)
  (kernel-syntax-case (syntax-disarm expr code-insp) #f
    
    [(#%plain-lambda formals subexpr ...)
     (quasisyntax/loc expr
       (#%plain-lambda forms #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
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
     (quasisyntax/loc expr
       (quote #,(on-datum #'datum)))]

    [(quote-syntax datum)
     (quasisyntax/loc expr
       (quote-syntax #,(on-datum #'datum)))]

    [(with-continuation-mark key value body)
     (quasisyntax/loc expr
       (with-continuation-mark #,(on-expr #'key) #,(on-expr #'value) #,(on-expr #'body)))]
    
    [(#%plain-app subexpr ...)
     expr
     #;(displayln expr)
     #;(quasisyntax/loc expr
       (#%plain-app #,@(map on-expr (syntax->list #'(subexpr ...)))))]

    [(#%top . id)
     expr]
    
    [(#%variable-reference (#%top . id))
     expr]
    [(#%variable-reference id)
     expr]
    [(#%variable-reference)
     expr]))

        

(define (on-datum datum-stx)
  (printf "looking at datum: ~s\n" datum-stx)
  datum-stx)



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
