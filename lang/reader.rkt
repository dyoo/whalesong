#lang s-exp syntax/module-reader

;; http://docs.racket-lang.org/planet/hash-lang-planet.html

#:language (lambda (ip)
	    `(file ,(path->string whalesong-lang-path)))

(require racket/runtime-path)
(define-runtime-path whalesong-lang-path "whalesong.rkt")
