#lang s-exp "../../lang/wescheme.rkt"


"eof.rkt"

(check-expect (eof-object? eof) true)
(check-expect (eof-object? 'eof) false)

"eof.rkt end"