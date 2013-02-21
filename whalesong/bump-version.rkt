#lang racket/base

(require racket/runtime-path
	 racket/port
	 racket/list)

(define-runtime-path version.rkt "version.rkt")

(define version-text (call-with-input-file version.rkt port->string))

(define revised-text (regexp-replace #px"\\.(\\d+)"
				     version-text
				     (lambda (whole sub)
				       (string-append 
					"."
					(number->string 
					 (add1 (string->number sub)))))))
  
(call-with-output-file version.rkt (lambda (op) (display revised-text op))
		       #:exists 'replace)