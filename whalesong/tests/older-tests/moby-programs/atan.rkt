#lang s-exp "../../lang/wescheme.rkt"
"atan.rkt tests"

(define delta 0.000001)

(check-within (atan 0.5)
              0.4636476090008061
              delta)
(check-within (atan 2 1)
              1.1071487177940904
	      delta)
(check-within (atan -2 -1)
	      -2.0344439357957027
	      delta)
(check-within (real-part (atan 1.0+5.0i))
	      1.530881333938778
	      delta)

(check-within (imag-part (atan 1.0+5.0i))
	      0.19442614214700213
	      delta)

(check-within (atan +inf.0 -inf.0)
	      2.356194490192345
	      delta)

"atan.rkt tests done"