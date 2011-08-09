#lang racket/base

(require planet/version
         syntax/kerncase
         net/base64
         (for-template (this-package-in lang/kernel)
                       #;(this-package-in image/main))
         
         ;; FIXME: I don't quite understand why I should be doing a require
         ;; of the image library at compile time, and not at template time.
         (this-package-in image/main))



(provide expand-out-images)

;; my-image-url: (parameterof stx)
;;
;; During the dynamic extent of expand-out-images, this will be defined
;; as the unique name for the image-url function in (planet dyoo/whalesong/image).
(define my-image-url (make-parameter #f))


;; expand-out-images: syntax -> syntax
;; Takes programs and rips out their image snips in favor of calls to
;; image-url.
(define (expand-out-images stx)
  (define expanded (expand stx))
  
  ;; We need to translate image snips in the expanded form so we can
  ;; fruitfully use compiler/zo-parse.    
  (define rewritten
    (parameterize
        ([my-image-url (car (generate-temporaries #'(image-url)))])
      
      (kernel-syntax-case (syntax-disarm expanded code-insp) #f
        [(#%expression expr)
         (quasisyntax/loc stx
           (#%expression #,(on-expr #'expr)))]
        
        [(module id name-id (#%plain-module-begin module-level-form ...))
         (quasisyntax/loc stx
           (module id name-id (#%plain-module-begin 
                               ;; Kludge: I'm trying to get at the image-url
                               ;; function, but in a way that doesn't clash with the
                               ;; user's existing program.
                               (require (rename-in (planet dyoo/whalesong/image)
                                                   [image-url #,(my-image-url)]))
                               
                               #,@(map on-toplevel
                                       (syntax->list #'(module-level-form ...))))))]
        [(begin top-level-form ...)
         (quasisyntax/loc stx
           (begin #,@(map on-toplevel 
                          (syntax->list #'(top-level-form ...)))))]
        [else
         (on-toplevel expanded)])))
  rewritten)




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


(define (on-datum datum-stx on-regular-datum)
  (define-values (image? convert) 
    (values
     (dynamic-require '2htdp/image 'image?)
     (dynamic-require 'file/convertible 'convert)))
  
  ;; Translates image values to embeddable uris.  See:
  ;; http://en.wikipedia.org/wiki/Data_URI_scheme
  ;; This code is ripped out of the tracer library written by
  ;; Will Zimrin and Jeanette Miranda.
  ;; returns the data-uri encoding of an image.
  (define (image->uri img)
    (define base64-bytes (base64-encode (convert img 'png-bytes)))
    (string-append "data:image/png;charset=utf-8;base64,"
                   (bytes->string/utf-8 base64-bytes)))
  
  (cond
    [(image? (syntax-e datum-stx))
     ;; When we see an image, we replace it with a call to
     ;; our image-url function.
     (with-syntax ([image-uri 
                    (image->uri (syntax-e datum-stx))])
       (quasisyntax/loc datum-stx
         (#,(my-image-url) image-uri)))]
    
    [else
     (on-regular-datum datum-stx)]))



(define (on-toplevel stx)
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