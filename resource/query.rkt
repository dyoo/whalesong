#lang racket/base

(require racket/contract
         racket/runtime-path
         syntax/modresolve
         racket/path
         "structs.rkt")


(provide/contract [query (module-path? . -> . (listof resource?))])
                  
(define-runtime-path record.rkt "record.rkt")
(define ns (make-base-namespace))

;; query: module-path -> (listof record)
;; Given a module, collect all of its resource records
(define (query a-module-path)
  (let ([resolved-path (normalize-path (resolve-module-path a-module-path #f))])
    (parameterize ([current-namespace ns])
      (dynamic-require a-module-path (void)) ;; get the compile-time code running.
      ((dynamic-require-for-syntax record.rkt 'get-records) resolved-path))))
