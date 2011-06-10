#lang s-exp "../../lang/base.rkt"


(provide test test-values Section record-error arity-test err/rt-test disable
         exn:application:mismatch? exn:application:type? exn:application:arity?
	 report-errs type? arity?)

(require (for-syntax racket/base))

(define number-of-tests 0)
(define number-of-error-tests 0)


(define Section-prefix "")

(define cur-section '())

(define errs '())

(define (record-error e)
  (set! errs (cons (list cur-section e) errs)))


(define (Section . args)
  (let ()
    (printf "~aSection~s\n" Section-prefix args)
    #;(flush-output p))
  (set! cur-section args)
  #t)


(define (report-errs)
  (printf "\n\nran ~s normal tests, and ~s error-trapping tests\n"
          number-of-tests number-of-error-tests)
  (printf "\n\n~a errors during the run:\n" (length errs))
  (for-each (lambda (err) (printf "~s\n\n" err)) errs))



(define test
  (let ()
    (define (test* expect fun args kws kvs)
      (set! number-of-tests (add1 number-of-tests))
      (printf "~s ==> " (cons fun args))
      #;(flush-output)
      (let ([res (if (procedure? fun)
                   (if kws
		       (error 'test "keywords not supported yet")
		       (apply fun args))
                   (car args))])
        (printf "~s\n" res)
        (let ([ok? (equal? expect res)])
          (unless ok?
            (record-error (list res expect (cons fun args)))
            (printf "  BUT EXPECTED ~s\n" expect))
          ok?)))
    #;(define (test/kw kws kvs expect fun . args) (test* expect fun args kws kvs))
    (define (test    expect fun         . args) (test* expect fun args #f #f))
    test
    #;(make-keyword-procedure test/kw test)))



(define (test-values l thunk)
  (test l call-with-values thunk list))



(define exn:application:mismatch? exn:fail:contract?)
(define exn:application:type? exn:fail:contract?)
(define exn:application:arity? exn:fail:contract:arity?)

(define type? exn:application:type?)
(define arity? exn:application:arity?)



(define arity-test
  (case-lambda
   [(f min max except)
    #;(void)
    (letrec ([aok?
	      (lambda (a)
		(cond
		 [(integer? a) (= a min max)]
		 [(arity-at-least? a) (and (negative? max)
					   (= (arity-at-least-value a) min))]
		 [(and (list? a) (andmap integer? a))
		  (and (= min (car a)) (= max
					  (let loop ([l a])
					    (if (null? (cdr l))
						(car l)
						(loop (cdr l))))))]
		 [(list? a)
		  ;; Just check that all are consistent for now.
		  ;; This should be improved.
		  (andmap
		   (lambda (a)
		     (if (number? a)
			 (<= min a (if (negative? max) a max))
			 (>= (arity-at-least-value a) min)))
		   a)]
		 [else #f]))]
	     [make-ok?
                (lambda (v)
                  (lambda (e)
                    (exn:application:arity? e)))]
	     [do-test
	      (lambda (f args check?)
		(set! number-of-error-tests (add1 number-of-error-tests))
		(printf "(apply ~s '~s)  =e=> " f args)
                
                (let/cc done
		  (let ([v (with-handlers ([void
					    (lambda (exn)
					      (if (check? exn)
						  (printf " ~a\n" (if (exn? exn)
                                                                      (exn-message exn)
                                                                      (format "uncaught ~x" exn)))
						  (let ([ok-type? (exn:application:arity? exn)])
						    (printf " WRONG EXN ~a: ~s\n"
							    (if ok-type?
								"FIELD"
								"TYPE")
							    exn)
						    (record-error (list exn
									(if ok-type?
									    'exn-field
									    'exn-type)
									(cons f args)))))
					      (done (void)))])
			     (apply f args))])
		    (printf "~s\n BUT EXPECTED ERROR\n" v)
		    (record-error (list v 'Error (cons f args))))))])
      (let loop ([n 0][l '()])
	(unless (>= n min)
	  (unless (memq n except)
	    (do-test f l (make-ok? n)))
	  (loop (add1 n) (cons 1 l))))
      (let loop ([n min])
	(unless (memq n except)
	  (test #t procedure-arity-includes? f n))
	(unless (>= n max)
	  (loop (add1 n))))
      (if (>= max 0)
	  (do-test f (let loop ([n 0][l '(1)])
		       (if (= n max)
			   l
			   (loop (add1 n) (cons 1 l))))
		   (make-ok? (add1 max)))
	  (test #t procedure-arity-includes? f 1267650600228229401496703205376 #;(arithmetic-shift 1 100))))]
   [(f min max) (arity-test f min max null)]))



;; err/rt-test currently a stub that doesn't do anything.
#;(define-syntax err/rt-test
  (lambda (stx)
    (syntax-case stx ()
      [(_ e exn?)
       (syntax
	(thunk-error-test (err:mz:lambda () e) (quote-syntax e) exn?))]
      [(_ e)
       (syntax
	(err/rt-test e exn:application:type?))])))


(define-syntax (err/rt-test stx)
  (syntax-case stx ()
    [(_ e exn?)
     (with-syntax ([stx-datum (syntax->datum #'e)])
       (syntax/loc stx
         (err/rt-test-helper (lambda () e) exn? (quote stx-datum))))]
    [(_ e)
     (syntax/loc stx
       (err/rt-test e exn:application:type?))]))



(define (err/rt-test-helper thunk exn? datum)
  (set! number-of-error-tests (add1 number-of-tests))
  (with-handlers ([exn? (lambda (exn) (void))])
    (thunk)
    (record-error (list 'Error datum)))
  #;(printf "took ~s milliseconds\n" (- (current-inexact-milliseconds) start-time)))



(define-syntax (disable stx)
  (syntax-case stx ()
    [(_ e ...)
     (syntax/loc stx
       (void))]))