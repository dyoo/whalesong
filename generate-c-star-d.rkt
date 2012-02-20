#lang racket

(provide make-c*r names)

;; A little helper to generate the tedious code for the c*r functions.

(define (make-c*r (n 4))
  (define template #<<EOF
installPrimitiveProcedure(
  '~a',
   1,
   function(M) {
       var x = M.e[M.e.length-1];
       if (isPair(x)&&~a) {
           return x.~a;
       } else {
           raiseArgumentTypeError(M, ~s, ~s, 0, x);
       }
   });

EOF

    )
  (for/list ([desc (combinations n)])
    (format template
            (string-append "c" desc "r")
            (test desc)
            (accessor desc)
            (string-append "c" desc "r")
            (string-append "c" desc "rable value"))))


(define (names (n 4))
  (for/list ([desc (combinations n)])
    (string->symbol (string-append "c" desc "r"))))




(define (combinations n)
  (define (helper n)
    (let loop ([n n])
      (cond
        [(= n 1)
         (list "a" "d")]
        [else
         (define sub-answers (loop (sub1 n)))
         (append (map (lambda (x) (string-append x "a")) sub-answers)
                 (map (lambda (x) (string-append x "d")) sub-answers))])))
  (apply append (for/list ([n (in-range 2 (add1 n))])
                  (helper n))))
  
 
(define (accessor s)
  (string-join 
   (reverse (for/list ([ch s])
              (case ch
                [(#\a) "first"]
                [(#\d) "rest"])))
   "."))

(define (test s)
  (string-join (for/list ([i (in-range 1 (string-length s))])
                 (let ([s (substring s 
                                     (- (string-length s) i)
                        (string-length s))])
                   (format "isPair(x.~a)" 
                           (string-join 
                            (reverse (for/list ([ch s])
                          (case ch
                            [(#\a) "first"]
                            [(#\d) "rest"])))
                            "."))))
               "&&"))
  
