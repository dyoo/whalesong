#lang s-exp "../../lang/base.ss"


(printf "when-unless.rkt\n")

(when (= (expt 2 100)
	 1267650600228229401496703205376)
  'ok)

(unless (not (= (expt 2 100)
	   1/1267650600228229401496703205376))
  (error 'not-ok))