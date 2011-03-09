#lang racket

(require "assemble.rkt"
         "browser-evaluate.rkt"
         "parse.rkt"
         "il-structs.rkt"
         racket/port
         racket/runtime-path)

(define-runtime-path runtime.js "runtime.js")
(define runtime (call-with-input-file runtime.js
                  (lambda (ip) (port->string ip))))

; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expr expected)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running ~s ...\n" 'exp)
           (let ([actual expr])
             (unless (equal? actual expected)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                   #'stx))
             (printf "ok.\n\n")))))]))



(define -E (make-evaluate
           (lambda (a-statement+inspector op)
             (let* ([a-statement (car a-statement+inspector)]
                    [inspector (cdr a-statement+inspector)]
                    [code
                     (string-append
                      "(function() { "
                      runtime
                      "return function(success, fail, params){" (assemble-statement a-statement) 
                      (format "success(String(~a)); };" inspector)
                      "});")])
               (display code op)))))

(define (E a-statement (inspector "MACHINE.val"))
  (evaluated-value (-E (cons a-statement inspector))))



(test (E (make-AssignImmediateStatement 'val (make-Const 42)))
      "42")
(test (E (make-AssignImmediateStatement 'val (make-Const "Danny")))
      "Danny")