#lang racket
(require rackunit)

(provide parse-expr)

;; While loops...
(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))


;; ignorable-next-char?: input-port -> boolean
;; Produces true if the next character is something we should ignore.
(define (ignorable-next-char? in)
  (let ([next-ch (peek-char in)])
    (cond
      [(eof-object? next-ch)
       #f]
      [else
       (not (member next-ch '(#\< #\> #\+ #\- #\, #\. #\[ #\])))])))


;; parse-expr: any input-port -> (U syntax eof)
;; Either produces a syntax object or the eof object.
(define (parse-expr source-name in)
  (while (ignorable-next-char? in) (read-char in))
  (let*-values ([(line column position) (port-next-location in)]
                [(next-char) (read-char in)])
    
    ;; We'll use this function to generate the syntax objects by
    ;; default.
    ;; The only category this doesn't cover are brackets.
    (define (default-make-syntax type)
      (datum->syntax #f 
                     (list type)
                     (list source-name line column position 1)))
    (cond
      [(eof-object? next-char) eof]
      [else
       (case next-char
         [(#\<) (default-make-syntax 'less-than)]
         [(#\>) (default-make-syntax 'greater-than)]
         [(#\+) (default-make-syntax 'plus)]
         [(#\-) (default-make-syntax 'minus)]
         [(#\,) (default-make-syntax 'comma)]
         [(#\.) (default-make-syntax 'period)]
         [(#\[)
          ;; The slightly messy case is bracket.  We keep reading
          ;; a list of exprs, and then construct a wrapping bracket
          ;; around the whole thing.
          (let*-values ([(elements) (parse-exprs source-name in)]
                        [(following-line following-column 
                                         following-position) 
                         (port-next-location in)])
            (datum->syntax #f 
                           `(brackets ,@elements)
                           (list source-name
                                 line 
                                 column 
                                 position 
                                 (- following-position
                                    position))))]
         [(#\])
          eof])])))


;; parse-exprs: input-port -> (listof syntax)
;; Parse a list of expressions.
(define (parse-exprs source-name in)
  (let ([next-expr (parse-expr source-name in)])
    (cond
      [(eof-object? next-expr)
       empty]
      [else
       (cons next-expr (parse-exprs source-name in))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple tests
(check-equal? eof (parse-expr 'test (open-input-string "")))
(check-equal? '(greater-than) 
              (syntax->datum (parse-expr 'test (open-input-string ">"))))
(check-equal? '(less-than) 
              (syntax->datum (parse-expr 'test (open-input-string "<"))))
(check-equal? '(plus) 
              (syntax->datum (parse-expr 'test (open-input-string "+"))))
(check-equal? '(minus)
              (syntax->datum (parse-expr 'test (open-input-string "-"))))
(check-equal? '(comma)
              (syntax->datum (parse-expr 'test (open-input-string ","))))
(check-equal? '(period)
              (syntax->datum (parse-expr 'test (open-input-string "."))))


;; bracket tests
(check-equal? '(brackets) 
              (syntax->datum (parse-expr 'test (open-input-string "[]"))))
(check-equal? '(brackets (brackets))
              (syntax->datum (parse-expr 'test (open-input-string "[[]]"))))


;; Parsing the "cat" function
(let ([port (open-input-string ",[.,]")])
  (check-equal? '(comma) 
                (syntax->datum (parse-expr 'test port)))
  (check-equal? '(brackets (period) (comma))
                (syntax->datum (parse-expr 'test port)))
  (check-equal? eof
                (parse-expr 'test port)))