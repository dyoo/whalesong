#lang s-exp "../../lang/wescheme.ss"

(printf "quasiquote.rkt\n")

(define name "danny")
(define roommates (list "guillaume" "isis" "andy"))

(check-expect `(my name is ,name and I lived with ,@roommates)
	      '(my name is "danny" and I lived with "guillaume" "isis" "andy"))