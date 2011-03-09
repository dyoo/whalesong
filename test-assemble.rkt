#lang racket

(require "assemble.rkt"
         "browser-evaluate.rkt"
         "parse.rkt"
         "il-structs.rkt"
         racket/port
         racket/promise
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
           (printf "Running ~s ...\n" (syntax->datum #'expr))
           (let ([actual expr])
             (unless (equal? actual expected)
               (raise-syntax-error #f (format "Expected ~s, got ~s" exp actual)
                                   #'stx))
             (printf "ok.\n\n")))))]))


;; evaluating single expression
(define -E (delay (make-evaluate
           (lambda (a-statement+inspector op)
             (let* ([a-statement (car a-statement+inspector)]
                    [inspector (cdr a-statement+inspector)]
                    [snippet (assemble-statement a-statement)]
                    [code
                     (string-append
                      "(function() { "
                      runtime
                      "return function(success, fail, params){" snippet
                      (format "success(String(~a)); };" inspector)
                      "});")])
               (displayln snippet)
               (display code op))))))
(define (E-single a-statement (inspector "MACHINE.val"))
  (evaluated-value ((force -E) (cons a-statement inspector))))

;; evaluating many expressions[.
(define -E-many (delay (make-evaluate
           (lambda (a-statement+inspector op)
             (let* ([a-statement (car a-statement+inspector)]
                    [inspector (cdr a-statement+inspector)])

               (display "(function() { " op)
               (display runtime op)
               
               (display "var myInvoke = " op)
               (assemble/write-invoke a-statement op)
               (display ";" op)
               
               (fprintf op 
                        "return function(succ, fail, params) { console.log('here'); myInvoke(function(v) { console.log('there!');succ(String(~a));}, fail, params); }"
                        inspector)
               (display "})" op))))))
(define (E-many stmts (inspector "MACHINE.val"))
  (evaluated-value ((force -E-many) (cons stmts inspector))))








;; Assigning a number
(test (E-single (make-AssignImmediateStatement 'val (make-Const 42)))
      "42")
;; Assigning a string
(test (E-single (make-AssignImmediateStatement 'val (make-Const "Danny")))
      "Danny")
;; Assigning a cons
(test (E-single (make-AssignImmediateStatement 'val (make-Const (cons 1 2))))
      "1,2")
;; Assigning to proc means val should still be uninitialized.
(test (E-single (make-AssignImmediateStatement 'proc (make-Const "Danny")))
      "undefined")
;; But we should see the assignment if we inspect MACHINE.proc.
(test (E-single (make-AssignImmediateStatement 'proc (make-Const "Danny"))
         "MACHINE.proc")
      "Danny")


(test (E-single (make-PushEnvironment 1)
         "MACHINE.env.length")
      "1")
(test (E-single (make-PushEnvironment 20)
                "MACHINE.env.length")
      "20")

(test (E-many (list (make-PushEnvironment 1))
              "MACHINE.env.length")
      "1")