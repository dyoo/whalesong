#lang racket/base

;; Function to get the runtime library.

(require racket/contract
         racket/runtime-path
         racket/port)


(provide/contract [get-runtime (-> string?)])
         
(define-runtime-path runtime.js "mini-runtime.js")

(define text (call-with-input-file runtime.js
               (lambda (ip)
                 (port->string ip))))

(define (get-runtime)
  text)