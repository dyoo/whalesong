#lang s-exp "../../lang/wescheme.rkt"

(require "double.rkt")

(check-expect (double 3) 6)

(check-expect (double (double (double 2))) 16)

