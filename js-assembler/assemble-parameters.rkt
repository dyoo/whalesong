#lang typed/racket/base
(require (for-syntax racket/base))

(provide lookup-string-table 
         with-fresh-string-table
         string-table)

(: lookup-string-table (String -> Natural))
(define (lookup-string-table a-str)
  (hash-ref! (string-table) a-str (lambda: () 
                                    (define result (unbox (counter)))
                                    (set-box! (counter) (add1 (unbox (counter))))
                                    result)))


(define-syntax (with-fresh-string-table stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(parameterize ([string-table ((inst make-hash String Natural))]
                      [counter ((inst box Natural) 0)])
         body ...)]))



(: string-table (Parameterof (HashTable String Natural)))
(define string-table (make-parameter ((inst make-hash String Natural))))

(: counter (Parameterof (Boxof Natural)))
(define counter (make-parameter ((inst box Natural) 0)))





