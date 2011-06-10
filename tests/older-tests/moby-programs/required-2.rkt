#lang s-exp "../../lang/base.ss"

(require "required-3.rkt")
(require "required-5.rkt")

(provide hypo
	 h)


(define-struct a-struct (x y z))
(provide (struct-out a-struct))


(provide (except-out (all-from-out "required-5.rkt")
		     clashing-value))