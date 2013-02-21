#lang racket/base


(provide lookup-language-namespace)




(define language-namespace-cache (make-hash))
;; lookup-language-namespace: module-path -> namespace
;; Returns a namespace associated with the lang.
(define (lookup-language-namespace lang)
  (hash-ref language-namespace-cache lang
            (lambda ()
              (let ([ns (make-base-empty-namespace)])
                (parameterize ([current-namespace ns])
                  (namespace-require lang))
                (hash-set! language-namespace-cache lang ns)
                ns))))
                    