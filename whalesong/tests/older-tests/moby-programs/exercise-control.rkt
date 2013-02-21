#lang s-exp "../../lang/wescheme.ss"

(printf "exercise-control.rkt\n")


(check-expect (if true
		  'ok
		  'not-ok)
	      'ok)

(check-expect (if false
		  'not-ok
		  'ok)
	      'ok)

(check-expect (cond [true 'ok]
		    [else 'not-ok])
	      'ok)

(check-expect (cond [false 'not-ok]
		    [else 'ok])
	      'ok)

(check-expect (case 42
		[(1) 'not-ok]
		[(2) 'not-ok]
		[(42) 'ok])
	      'ok)

(check-expect (case 42
		[(1) 'not-ok]
		[(2) 'not-ok]
		[(42) 'ok])
	      'ok)


;; Runtime error: we should see if the test isn't boolean
(with-handlers ([exn:fail? 
                 (lambda (exn)
                   (unless (string=? "cond: question result is not true or false: 42"
                                     (exn-message exn))
                     (error 'cond-test)))])
  (cond
    [42 
     (error 'uh-oh)]
    [else 
     (error 'cond-test)]))



;; Test fall-through
(with-handlers ([exn:fail? 
                 (lambda (exn)
                   (unless (string=? "cond: all question results were false"
                                     (exn-message exn))
                     (error 'cond-test)))])
  (cond
    [false (error 'uh-oh)]))



;; Runtime error: we should see if the test isn't boolean
(with-handlers ([exn:fail? 
                 (lambda (exn)
                   (unless (string=? "if: question result is not true or false: \"not a boolean\""
                                     (exn-message exn))
                     (error 'cond-test)))])
  (if "not a boolean" 
      (error 'uh-oh)
      (error 'uh-oh)))


;; Check fall-though with case being an error
(with-handlers ([exn:fail?
                 (lambda (exn)
                   (unless (string=? "case: the expression matched none of the choices"
                                     (exn-message exn))
                     (error 'case-test)))])
  (case 42
    [(1) (error 'case)]
    [(2) (error 'case)]
    [(3) (error 'case)])
  (error 'case))



(with-handlers ([exn:fail? 
                 (lambda (exn)
                   (unless (string=? "when: question result is not true or false: \"not a boolean\""
                                     (exn-message exn))
                     (error 'when-boolean-test)))])
  (when "not a boolean" 
    (error 'uh-oh)))



(with-handlers ([exn:fail? 
                 (lambda (exn)
                   (unless (string=? "unless: question result is not true or false: \"not a boolean\""
                                     (exn-message exn))
                     (error 'unless-boolean-test)))])
  (unless "not a boolean" 
    (error 'uh-oh)))




(unless (= 0 0)
  (error 'huh?))

(when (= 0 1)
  (error 'huh?))



(check-expect (let/cc return
	      	(begin
		  (return 42)
		  (error)))
	      42)


(check-expect (let/cc return
	      	(begin
		  'fall-through))
	      'fall-through)




(printf "exercise-control.rkt end\n")


