#lang typed/racket/base
(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "lexical-env.rkt"
         "helpers.rkt"
         "find-toplevel-variables.rkt"
         "sets.rkt"
         "compile.rkt"
         "typed-parse.rkt"
         racket/list)



(provide get-bootstrapping-code)



;; The primitive code necessary to do call/cc

(: call/cc-label Symbol)
(define call/cc-label 'callCCEntry)
(define call/cc-closure-entry 'callCCClosureEntry)


;; (call/cc f)
;; Tail-calls f, providing it a special object that knows how to do the low-level
;; manipulation of the environment and control stack.
(define (make-call/cc-code)
  (statements
   (append-instruction-sequences
    (make-instruction-sequence 
     `(,call/cc-label
       ;; Precondition: the environment holds the f function that we want to jump into.
       
       ;; First, move f to the proc register
       ,(make-AssignImmediateStatement 'proc (make-EnvLexicalReference 0 #f))
       
       ;; Next, capture the envrionment and the current continuation closure,.
       ,(make-PushEnvironment 2 #f)
       ,(make-AssignPrimOpStatement (make-EnvLexicalReference 0 #f) 
                                    (make-CaptureControl 0))
       ,(make-AssignPrimOpStatement (make-EnvLexicalReference 1 #f)
                                    ;; When capturing, skip over f and the two slots we just added.
                                    (make-CaptureEnvironment 3))
       ,(make-AssignPrimOpStatement (adjust-target-depth (make-EnvLexicalReference 0 #f) 2)
                                    (make-MakeCompiledProcedure call/cc-closure-entry
                                                                1 ;; the continuation consumes a single value
                                                                (list (make-EnvLexicalReference 0 #f)
                                                                      (make-EnvLexicalReference 1 #f))
                                                                'call/cc))
       ,(make-PopEnvironment 2 0)))
    
    ;; Finally, do a tail call into f.
    (compile-procedure-call '()
                            (extend-lexical-environment/placeholders '() 1)
                            1 
                            'val
                            'return)
    
    ;; The code for the continuation coe follows.  It's supposed to
    ;; abandon the current continuation, initialize the control and environment, and then jump.
    (make-instruction-sequence `(,call/cc-closure-entry
                                 ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
                                 ,(make-PerformStatement (make-InstallClosureValues!))
                                 ,(make-PerformStatement (make-RestoreControl!))
                                 ,(make-PerformStatement (make-RestoreEnvironment!))
                                 ,(make-AssignPrimOpStatement 'proc (make-GetControlStackLabel))
                                 ,(make-PopControlFrame)
                                 ,(make-GotoStatement (make-Reg 'proc)))))))

(: make-bootstrapped-primitive-code (Symbol Any -> (Listof Statement)))
(define (make-bootstrapped-primitive-code name src)
  (parameterize ([current-defined-name name])
    (append
     (compile (parse src) (make-PrimitivesReference name) 'next)
     ;; Remove the prefix after the Primitives assignment.
     `(,(make-PopEnvironment 1 0)))))





(: get-bootstrapping-code (-> (Listof Statement)))
(define (get-bootstrapping-code)
  
  (append
   
   (make-bootstrapped-primitive-code 
    'map 
    '(letrec ([map (lambda (f l)
		     (if (null? l)
			 null
			 (cons (f (car l))
			       (map f (cdr l)))))])
       map))

   (make-bootstrapped-primitive-code
    'for-each
    '(letrec ([for-each (lambda (f l)
			  (if (null? l)
			      null
			      (begin (f (car l))
				     (for-each f (cdr l)))))])
       for-each))

   (make-bootstrapped-primitive-code
    'caar
    '(lambda (x)
       (car (car x))))


   (make-bootstrapped-primitive-code
    'memq
    '(letrec ([memq (lambda (x l)
		      (if (null? l)
			  #f
			  (if (eq? x (car l))
			      l
			      (memq x (cdr l)))))])
       memq))

   (make-bootstrapped-primitive-code
    'assq
    '(letrec ([assq (lambda (x l)
		     (if (null? l)
			 #f
			 (if (eq? x (caar l))
			     (car l)
			     (assq x (cdr l)))))])
      assq))

   (make-bootstrapped-primitive-code
    'length
    '(letrec ([length-iter (lambda (l i)
			    (if (null? l)
				i
				(length-iter (cdr l) (add1 i))))])
      (lambda (l) (length-iter l 0))))
			  

   (make-bootstrapped-primitive-code
    'append
    '(letrec ([append (lambda (l1 l2)
			(if (null? l1) 
			    l2
			    (cons (car l1) (append (cdr l1) l2))))])
       append))
	      

   
   ;; The call/cc code is special:
   (let ([after-call/cc-code (make-label 'afterCallCCImplementation)])
     (append 

      `(,(make-AssignPrimOpStatement (make-PrimitivesReference 'call/cc) 
                                     (make-MakeCompiledProcedure call/cc-label 1 '() 'call/cc))
        ,(make-AssignPrimOpStatement (make-PrimitivesReference 'call-with-current-continuation) 
                                     (make-MakeCompiledProcedure call/cc-label 1 '() 'call/cc))
        ,(make-GotoStatement (make-Label after-call/cc-code)))
      (make-call/cc-code)
      `(,after-call/cc-code)))))