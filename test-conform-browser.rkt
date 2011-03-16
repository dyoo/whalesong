#lang racket
(require "browser-evaluate.rkt"
         "package.rkt")

(define evaluate (make-evaluate package-anonymous))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "running test...")
           (let ([result (evaluate s)])
             (let ([output (evaluated-stdout result)])
               (unless (string=? output exp)
                 (printf " error!\n")
                 (raise-syntax-error #f (format "Expected ~s, got ~s" exp output)
                                     #'stx)))
             (printf " ok (~a milliseconds)\n" (evaluated-t result))))))]))


(test (read (open-input-file "tests/conform/program0.sch"))
      (port->string (open-input-file "tests/conform/expected0.txt")))