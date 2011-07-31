#lang racket/base

;; Provides a mapping of the core bindings in kernel, so that we know statically
;; if something is implemented as a primitive or a closure.
(require planet/util)


(define ns (make-base-empty-namespace))
(define bound-procedure-names
  (let ([path (resolve-planet-path `(planet ,(this-package-version-symbol lang/kernel)))])
    (parameterize ([current-namespace ns])
      (namespace-require path)
      (for/list ([name (namespace-mapped-symbols)]
                 #:when (namespace-variable-value name #t (lambda () #f)))
        name))))


