#lang racket/base

(provide repl-compile)

(define this-namespace (make-base-empty-namespace))

;; Somewhat magical.
;; See: http://lists.racket-lang.org/users/archive/2013-February/056664.html.
(define make-fresh-namespace 
  (eval '(lambda ()
           (variable-reference->empty-namespace
            (#%variable-reference)))
        (make-base-namespace)))


;; make-repl-namespace: [module-path] -> namespace
;; Creates a clean namespace for the given module path.
;;
;; Note that we cache prior instantiations of the language
;; to speed up construction of the namespace,
;; so don't let people call make-repl-namespace with arbitrary values.
(define (make-repl-namespace [language-module-path 'racket/base])
  (parameterize ([current-namespace this-namespace])
    (dynamic-require language-module-path 0))
  (define ns (make-fresh-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module this-namespace language-module-path)  
    (namespace-require language-module-path))
  ns)


;; repl-compile: any [#:lang module-path] -> compiled-bytecode
;; Compiles the given body in a toplevel context under the given language.
;; Compilation creates a fresh namespace each time to avoid one compilation
;; affecting the other.
;;
;; Note however, that the languages have to make sure not to maintain compilation
;; state themselves, since we reuse the language module to improve repl construction
;; time.
(define (repl-compile body #:lang [language-module-path 'racket/base])
  (parameterize ([current-namespace (make-repl-namespace language-module-path)])
    (compile body)))



;(for ([i 10])
;  (time (repl-compile '(* x 3) #:lang 'whalesong/lang/whalesong))
;  (time (repl-compile '(/ x 3) #:lang 'whalesong/lang/whalesong)))
