#lang racket/base
 
(require racket/port
         racket/contract)
 
(provide (contract-out [modularize-input-port (-> 
                                               #:module-name symbol?
                                               #:input-port input-port?
                                               #:semantics-module symbol? 
                                               input-port?)]))
                                
;; Add a module wrapper around s-expression-based toplevel code, but
;; preserving original source locations as best as we can.
(define (modularize-input-port #:module-name name    ;; symbol
                               #:input-port ip       ;; input port
                               #:semantics-module semantics-module)          ;; symbol
  (define header (format "(module ~s ~s\n" name semantics-module))
  (define lang-ip (open-input-string header))
  
  (define concatenated-port (input-port-append #f lang-ip ip (open-input-string ")")))
  (define (count-concatenated-port)
    (port-count-lines! concatenated-port))

  (define (get-location)
    (define-values (line column position) (port-next-location concatenated-port))
    (cond [(not (and line column position))
           (values #f #f #f)]
          [(<= position (string-length header))
           (values #f #f #f)]
          [else
           (port-next-location ip)]))
  
  (define-values (starting-line starting-col starting-pos)
    (port-next-location ip))
  (transplant-input-port concatenated-port 
                         get-location 
                         (or starting-pos 1)
                         #f 
                         count-concatenated-port))
 
 
(module* test racket/base
  (require (submod "..")
           racket/port)
  (define original-ip (open-input-string "(+ 1\n 2)"))
  (port-count-lines! original-ip)
  (define relocated-ip (relocate-input-port original-ip 5 1 200))
  (port-count-lines! relocated-ip)
  (define new-ip (modularize-input-port #:module-name 'test
                                        #:input-port relocated-ip #;original-ip
                                        #:semantics-module 'wescheme))
  (port-count-lines! new-ip)
  (read-syntax #f new-ip))
