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
               (raise-syntax-error #f (format "Expected ~s, got ~s" expected actual)
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
                        "return function(succ, fail, params) { myInvoke(function(v) { succ(String(~a));}, fail, params); }"
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

;; PopEnvironment
(test (E-many (list (make-PushEnvironment 2))
              "MACHINE.env.length")
      "2")
(test (E-many (list (make-PushEnvironment 2)
                    (make-PopEnvironment 1 0))
              "MACHINE.env.length")
      "1")



;; Assigning to the environment
(test (E-many (list (make-PushEnvironment 2)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0)
                                                   (make-Const 12345)))
              "MACHINE.env[1]")
      "12345")
(test (E-many (list (make-PushEnvironment 2)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0)
                                                   (make-Const 12345)))
              "MACHINE.env[0]")
      "undefined")
(test (E-many (list (make-PushEnvironment 2)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1)
                                                   (make-Const 12345)))
              "MACHINE.env[0]")
      "12345")

;; A do-nothing closure
(test (E-many (list (make-GotoStatement (make-Label 'afterLambda))
                    'closureStart
                    (make-GotoStatement (make-Label 'afterLambda))
                    'afterLambda
                    (make-AssignPrimOpStatement 'val (make-MakeCompiledProcedure 'afterLambda 0 '())))
              "MACHINE.val.displayName")
      "afterLambda")



;; A do-nothing closure with a few values
(test (E-many (list (make-GotoStatement (make-Label 'afterLambda))
                    'closureStart
                    (make-GotoStatement (make-Label 'afterLambda))
                    'afterLambda
                    (make-PushEnvironment 2)
                    (make-AssignImmediateStatement (make-EnvLexicalReference 0)
                                                   (make-Const "hello"))
                    (make-AssignImmediateStatement (make-EnvLexicalReference 1)
                                                   (make-Const "world"))
                    (make-AssignPrimOpStatement 'val (make-MakeCompiledProcedure 'afterLambda 0 
                                                                                 (list (make-EnvLexicalReference 0)
                                                                                       (make-EnvLexicalReference 1)))))
              "MACHINE.val.closedVals[0] + ',' + MACHINE.val.closedVals[1]")
      "hello,world")