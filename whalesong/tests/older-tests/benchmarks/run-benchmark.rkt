#lang s-exp "../../lang/base.rkt"

(provide run-benchmark)


;;; Gambit-style run-benchmark.
;;;
;;; Invoke this procedure to run a benchmark.
;;; The first argument is a string identifying the benchmark.
;;; The second argument is the number of times to run the benchmark.
;;; The third argument is a thunk that runs the benchmark.
;;; The fourth argument is a unary predicate that warns if the result
;;; returned by the benchmark is incorrect.
;;;
;;; Example:
;;; (run-benchmark "make-vector"
;;;                1
;;;                (lambda () (make-vector 1000000))
;;;                (lambda (v) (and (vector? v) (= (vector-length v) #e1e6))))

;;; For backward compatibility, this procedure also works with the
;;; arguments that we once used to run benchmarks in Larceny.
 
(define (run-benchmark name arg2 . rest)
  (let* ((old-style (procedure? arg2))
         (thunk (if old-style arg2 (car rest)))
         (n (if old-style
                (if (null? rest) 1 (car rest))
                arg2))
         (ok? (if (or old-style (null? (cdr rest)))
                  (lambda (result) #t)
                  (cadr rest)))
         (result '*))
    (define (loop n)
      (cond ((zero? n) #t)
            ((= n 1)
             (set! result (thunk)))
            (else
             (thunk)
             (loop (- n 1)))))
    (when old-style
        (begin (newline)
               (display "Warning: Using old-style run-benchmark")
               (newline)))
    (newline)
    (display "--------------------------------------------------------")
    (newline)
    (display name)
    (newline)
    ; time is a macro supplied by Chez Scheme
    (time (loop n))
    (when (not (ok? result))
        (begin (display "Error: Benchmark program returned wrong result: ")
               (write result)
               (newline)))))

