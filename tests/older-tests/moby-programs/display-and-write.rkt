#lang s-exp "../../lang/wescheme.rkt"


(printf "should not be in quotes: ")
(display "hello world")
(newline)
(printf "should be in quotes: ") 
(write "hello world")
(newline)


((current-print) "using current-print")
