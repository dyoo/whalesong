#lang s-exp "../lang/base.rkt"

(require "impl.rkt"
         "helpers.rkt"
         "event.rkt"
         (for-syntax racket/base))

(require (for-syntax racket/base racket/stxparam-exptime)
         (only-in "../lang/kernel.rkt" define-syntax-parameter syntax-parameterize))

(provide (except-out (all-from-out "impl.rkt")
                     big-bang
                     initial-view
                     stop-when
                     on-tick
                     on-mock-location-change
                     on-location-change
                     to-draw)
         (all-from-out "helpers.rkt")
         (all-from-out "event.rkt"))

(provide view-bind*)

(provide (rename-out [internal-big-bang big-bang]
                     [big-bang big-bang/f]

                     
                     [initial-view initial-view/f]
                     [stop-when stop-when/f]

                     [on-tick on-tick/f]

                     [on-mock-location-change on-mock-location-change/f]

                     [on-location-change on-location-change/f]

                     [to-draw to-draw/f]))

(define-syntax-parameter in-big-bang? #f)

(define-syntax (internal-big-bang stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx (big-bang (syntax-parameterize ([in-big-bang? #t])
                                                    body)
                               ...))]
    [else
     (raise-syntax-error #f "big-bang should be applied")]))

(define-syntax (define/provide-protected stx)
  (syntax-case stx ()
    [(_ (real-function ...))
     (with-syntax ([(internal-name ...) 
                    (generate-temporaries (syntax->list #'(real-function ...)))])
       (syntax/loc stx       
         (begin (begin (define-syntax (internal-name stx2)
                         (syntax-case stx2 ()
                           [(_ args (... ...))
                            (cond
                              [(syntax-parameter-value #'in-big-bang?)
                               
                               (syntax/loc stx2
                                 (real-function args (... ...)))]
                              [else
                               (raise-syntax-error #f (format "~a should be applied in the context of a big-bang"
                                                              'real-function)
                                                   stx2)])]
                           [else
                            (raise-syntax-error #f 
                                                (format "~a should be applied in the context of a big-bang"
                                                        'real-function)
                                                stx2)]))
                       (provide (rename-out (internal-name real-function)))) ...)))]))

(define/provide-protected (initial-view
                           stop-when
                           on-tick
                           on-mock-location-change
                           on-location-change
                           to-draw))
  


;; A syntactic form to make it more convenient to focus and bind multiple things
;; (view-bind* a-view
;;             [id type function]
;;             [id type function] ...)
(define-syntax (view-bind* stx)
  (syntax-case stx ()
    [(_ a-view [a-selector a-type a-function] ...)
     (foldl (lambda (a-selector a-type a-function a-view-stx)
              #'(view-bind (view-focus #,a-view-stx #,a-selector)
                           #,a-type
                           #,a-function))
            #'a-view
            (syntax->list #'(a-selector ...))
            (syntax->list #'(a-type ...))
            (syntax->list #'(a-function ...)))]))
