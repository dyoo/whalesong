#lang s-exp "../../lang/wescheme.rkt"
(require "../../lang/check-expect/test-expect.rkt")

"values.rkt"

(call-with-values (lambda () (values 3 4 5))
                             (lambda (x y z)
			       (test-expect x 3)
			       (test-expect y 4)
			       (test-expect z 5)))

(call-with-values (lambda () (values 3 4 5))
                             (lambda args
			       (test-expect args '(3 4 5))))

(call-with-values (lambda () (values))
                             (lambda ()
			       (void)))



"values.rkt end"