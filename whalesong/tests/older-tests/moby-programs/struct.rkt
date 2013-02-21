#lang s-exp "../../lang/wescheme.rkt"

(require "../../lang/check-expect/test-expect.rkt")
"struct.rkt"

(let-values ([(a-struct-type
               constructor
               predicate
               accessor
               mutator)
              (make-struct-type 'pair 
                                #f
                                2
                                0)])
  (test-expect (struct-type? a-struct-type) true)
  (test-expect (struct-type? (constructor 3 4)) false)
  (test-expect (predicate (constructor 3 4)) true)
  (test-expect (predicate (cons 3 4)) false)

  (test-expect (struct-constructor-procedure? constructor) true)
  (test-expect (struct-constructor-procedure? (lambda (x y)
                                                 (constructor x y)))
                false)
  
  (test-expect (struct-predicate-procedure? predicate) true)
  (test-expect (struct-predicate-procedure? accessor) false)
  (test-expect (struct-predicate-procedure? 24) false)
  (test-expect (struct-predicate-procedure? (lambda (x) true))
                                             false)

  (test-expect (struct-accessor-procedure? accessor) true)
  (test-expect (struct-accessor-procedure? mutator) false)
  (test-expect (struct-accessor-procedure? 24) false)
  (test-expect (struct-accessor-procedure? (lambda (x) true))
                                             false)

  (test-expect (struct-mutator-procedure? mutator) true)
  (test-expect (struct-mutator-procedure? accessor) false)
  (test-expect (struct-mutator-procedure? 24) false)
  (test-expect (struct-mutator-procedure? (lambda (x) true))
                                             false)

  (let ([f (make-struct-field-accessor accessor 0)]
        [r (make-struct-field-accessor accessor 1)]
        [set-f! (make-struct-field-mutator mutator 0)]
        [set-r! (make-struct-field-mutator mutator 1)])
    (let ([p1 (constructor 17 'foo)])
      (test-expect (f p1) 17)
      (test-expect (r p1) 'foo)

      (set-f! p1 'something-else)
      (test-expect (f p1) 'something-else)
      (set-r! p1 1024)
      (test-expect (r p1) '1024))))

"struct.rkt end"