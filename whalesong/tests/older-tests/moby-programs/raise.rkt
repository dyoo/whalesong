#lang s-exp "../../lang/wescheme.rkt"

"raise.rkt"

(check-expect 
 (with-handlers ([string? identity])
   (raise "hello world")
   42)
 "hello world")


(check-expect (exn? (with-handlers ([void identity])
                      (raise (make-exn "foo" (current-continuation-marks)))))
              true)

(check-expect (exn:fail:contract:arity? (with-handlers ([void identity])
                      (+ "hello" "world")))
              false)

(check-expect (exn:fail:contract? (with-handlers ([void identity])
                      (+ "hello" "world")))
              true)

(check-expect (exn:fail:contract:arity? (with-handlers ([void identity])
                      (identity "hello" "world")))
              true)

(check-expect (exn:fail:contract:variable? (with-handlers ([void identity])
                      (identity "hello" "world")))
              false)



"raise.rkt end"