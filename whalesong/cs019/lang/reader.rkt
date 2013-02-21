#lang s-exp syntax/module-reader

;; http://docs.racket-lang.org/planet/hash-lang-planet.html

#:language (lambda (ip)
	    `(file ,(path->string cs019.rkt)))

(require racket/runtime-path)
(define-runtime-path cs019.rkt "../cs019.rkt")
