#lang s-exp "../../lang/wescheme.rkt"
(require "../../lang/check-expect/test-expect.rkt")

"continuation-marks.rkt"

(with-continuation-mark 'x 3
  (test-expect (continuation-mark-set->list 
                   (current-continuation-marks)
                   'x)
		'(3)))

(with-continuation-mark 'x 3
  (with-continuation-mark 'x 4
   (test-expect (continuation-mark-set->list 
                    (current-continuation-marks)
                    'x)
	         '(4))))

"continuation-marks.rkt end"