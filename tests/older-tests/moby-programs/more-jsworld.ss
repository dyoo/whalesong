#lang s-exp "../../lang/wescheme.rkt"

"more-jsworld.rkt"

;; Fill me in with automated tests for jsworld...
;; This file is intentionally with an '.ss' suffix
;; to see if we've also fixed an issue with module names.


;; The tests below make sure that mutable strings pose no issue
;; to the jsworld functions.


;; Thanks to William Zimrin and Tiberiu-Lucian Florea
;; for this test case.
(big-bang empty
          (to-draw-page 
	   (lambda (x)
	     (list (js-div '(("id" "div")))
		   (list (js-text (string-append "hello")))))
	   (lambda (x)
	     `((,(string-append "div") ,(list (string-append "border") (string-append "3px black solid"))))))
	  (stop-when (lambda (x) true)))






(local [
	(define (refresh w form-val)
	  form-val)

	(define input-node
	  (js-input (string-append "text") refresh '(("id" "myname"))))

	(define (draw w)
	  (list (js-div)
		(list (js-div) (list (js-text (format "I see: ~s~n" w))))
		(list (js-div) (list input-node))))

	(define (draw-css w)
	  '())]

  (big-bang ""
	    (to-draw-page draw draw-css)
	    (stop-when (lambda (x) true))))



(define true-f (lambda (x) true))


(let ([draw (lambda (w)
	      (list (js-img (string-append
			     "http://racket-lang.org/logo.png"))))])
  (big-bang 0
	    (to-draw-page draw)
	    (stop-when true-f)))





(local [
  (define (select-house w an-option)
    an-option)
  
  (define a-select-element
    (js-select (list (string-append "")
                     (string-append "Gryffindor")
                     "Hufflepuff"
                     "Ravenclaw"
                     (string-append "Slytherin"))
               select-house))
  
  (define (draw w)
    (list (js-div)
          (list a-select-element)
          (list (js-text (format "House: ~a" w)))))
  
  (define (draw-css w)
    '())]
  
  (big-bang ""
            (to-draw-page draw draw-css)
	    (stop-when true-f)))










(big-bang 0
	  (stop-when (lambda (x) true))
          (to-draw-page (lambda (x)
                          (list (js-div)
                                (list 
				 (js-input "checkbox" 
					   (lambda (x y) x)
					   '(("checked" true))));This checkbox is checked
                                (list
				 (js-input "checkbox"
					   (lambda (x y) x)
					   '(("checked" false))));This checkbox is checked
                                (list
				 (js-input "checkbox" 
					   (lambda (x y) x) 
					   '(("value" true))));This checkbox is not checked
                                (list
				 (js-input "checkbox"
					   (lambda (x y) x)
					   '(("value" false))))));This checkbox is not checked
                        (lambda (x) empty)))