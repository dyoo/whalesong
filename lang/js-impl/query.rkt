#lang racket/base

(require racket/contract
         racket/runtime-path
         racket/list
         syntax/modresolve)


(define-struct js-module (impls exports))

(provide/contract [query
                   (module-path? . -> . (or/c js-module? false/c))]
                  [struct js-module ([impls (listof string?)]
                                     [exports (listof symbol?)])])




(define-runtime-path record.rkt "record.rkt")
(define ns (make-base-empty-namespace))



;; query: module-path -> (listof string)
;; Given a module, see if it's implemented via Javascript.
(define (query a-module-path)
  (let ([resolved-path (resolve-module-path a-module-path #f)])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      (let ([result
             ((dynamic-require-for-syntax record.rkt 'lookup-implementations) resolved-path)])
        (cond
          [(empty? result)
           #f]
          [else
           (make-js-module result
                           ((dynamic-require-for-syntax record.rkt 'lookup-exports) resolved-path))])))))

  
