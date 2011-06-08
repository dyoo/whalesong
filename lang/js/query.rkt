#lang racket/base

(require racket/contract
         racket/runtime-path
         syntax/modresolve)


(provide/contract [query (module-path? . -> . string?)]
                  [has-javascript-implementation? (module-path? . -> . boolean?)]
                  
                  [redirected? (path? . -> . boolean?)]
                  [follow-redirection (path? . -> . path?)])

(define-runtime-path record.rkt "record.rkt")
(define ns (make-base-empty-namespace))

;; query: module-path -> string?
;; Given a module, see if it's implemented via Javascript.
(define (query a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      ((dynamic-require-for-syntax record.rkt 'lookup-javascript-implementation) resolved-path))))


;; has-javascript-implementation?:  module-path -> boolean
(define (has-javascript-implementation? a-module-path)  
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      ((dynamic-require-for-syntax record.rkt 'has-javascript-implementation?) resolved-path))))
  


;; redirected? path -> boolean
(define (redirected? a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      (path? ((dynamic-require-for-syntax record.rkt 'follow-redirection)
              resolved-path)))))


;; follow-redirection: module-path -> path
(define (follow-redirection a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      ((dynamic-require-for-syntax record.rkt 'follow-redirection)
       resolved-path))))

