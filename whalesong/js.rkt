#lang s-exp "lang/base.rkt"
(require "js/main.rkt")
(provide [except-out (all-from-out "js/main.rkt")
                     js-function->procedure
                     js-async-function->procedure]
         [rename-out [-js-function->procedure js-function->procedure]
                     [-js-async-function->procedure js-async-function->procedure]]
         js-function?)

(define raw-js-function?
  (js-function->procedure (js-eval "function(x) { return typeof(x) === 'function'}")))

(define (js-function? x)
  (raw-js-function? x))

(define (-js-function->procedure x)
  (cond
   [(string? x)
    (js-function->procedure (js-eval x))]
   [(js-function? x)
    (js-function->procedure x)]
   [else
    (raise-type-error 'js-function->procedure "js-function or string" x)]))

(define (-js-async-function->procedure x)
  (cond
   [(string? x)
    (js-async-function->procedure (js-eval x))]
   [(js-function? x)
    (js-async-function->procedure x)]
   [else
    (raise-type-error 'js-async-function->procedure "js-function or string" x)]))