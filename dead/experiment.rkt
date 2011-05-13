#lang racket/base
(require compiler/decompile
         compiler/zo-parse)

;; A little bit of code to see how Racket really compiles code.

(require scheme/pretty)

(provide try)

(define (try e)
  (let ([out (open-output-bytes)])
    (write (parameterize ([current-namespace (make-base-namespace)])
			 (compile e))
	   out)
    (let ([inp (open-input-bytes (get-output-bytes out))])

      (pretty-print
       (zo-parse inp)))))

