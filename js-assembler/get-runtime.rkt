#lang racket/base

;; Function to get the runtime library.

(require racket/contract
         racket/runtime-path
         racket/port)


(provide/contract [get-runtime (-> string?)])
         
(define-runtime-path jquery.js "runtime-src/jquery-1.6.1.min.js")
(define-runtime-path runtime.js "mini-runtime.js")


(define (path->string p)
  (call-with-input-file p
                (lambda (ip)
                  (port->string ip))))


(define text (string-append
              (path->string jquery.js)
              (path->string runtime.js)))




(define (get-runtime)
  text)