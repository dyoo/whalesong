#lang s-exp "../../lang/base.rkt"

(require "../../lang/location.rkt")
(require "../../lang/check-expect/check-expect.rkt")

"location.rkt"


(check-expect (location? (make-location "location.rkt" 88 7 0 37))
	      true)

"The following should be a location "
(make-location "location.rkt" 88 7 0 37)



"location.rkt end"
(run-tests)