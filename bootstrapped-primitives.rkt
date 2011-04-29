#lang typed/racket/base
(require "expression-structs.rkt"
         "lexical-structs.rkt"
         "il-structs.rkt"
         "compiler.rkt"
         "typed-parse.rkt"
         "parameters.rkt")


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
                                    (make-CaptureControl 0 default-continuation-prompt-tag))
       ,(make-AssignPrimOpStatement (make-EnvLexicalReference 1 #f)
                                    ;; When capturing, skip over f and the two slots we just added.
                                    (make-CaptureEnvironment 3 default-continuation-prompt-tag))
       ,(make-AssignPrimOpStatement (adjust-target-depth (make-EnvLexicalReference 0 #f) 2)
                                    (make-MakeCompiledProcedure call/cc-closure-entry
                                                                1 ;; the continuation consumes a single value
                                                                (list 0 1)
                                                                'call/cc))
       ,(make-PopEnvironment (make-Const 2) 
                             (make-Const 0))))
       
    ;; Finally, do a tail call into f.
    (make-instruction-sequence `(,(make-AssignImmediateStatement 'argcount (make-Const 1))))
    (compile-general-procedure-call '()
                                    (make-Const 1) ;; the stack at this point holds a single argument
                                    'val
                                    return-linkage)
    
    ;; The code for the continuation code follows.  It's supposed to
    ;; abandon the current continuation, initialize the control and environment, and then jump.
    (make-instruction-sequence `(,call/cc-closure-entry
                                 ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
                                 ,(make-PerformStatement (make-InstallClosureValues!))
                                 ,(make-PerformStatement 
                                   (make-RestoreControl! default-continuation-prompt-tag))
                                 ,(make-PerformStatement (make-RestoreEnvironment!))
                                 ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel))
                                 ,(make-PopControlFrame)
                                 ,(make-GotoStatement (make-Reg 'proc)))))))

(: make-bootstrapped-primitive-code (Symbol Any -> (Listof Statement)))
(define (make-bootstrapped-primitive-code name src)
  (parameterize ([current-defined-name name])
    (append
     (compile (parse src) (make-PrimitivesReference name) next-linkage/drop-multiple))))





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
    '(letrec ([append-many (lambda (lsts)
			     (if (null? lsts)
				 null
				 (if (null? (cdr lsts))
				     (car lsts)
				     (append-2 (car lsts)
					       (append-many (cdr lsts))))))]
	      [append-2 (lambda (l1 l2)
			  (if (null? l1) 
			      l2
			      (cons (car l1) (append-2 (cdr l1) l2))))])
       (lambda args (append-many args))))
   
   
   
   
   ;; The call/cc code is special:
   (let ([after-call/cc-code (make-label 'afterCallCCImplementation)])
     (append 

      `(,(make-AssignPrimOpStatement (make-PrimitivesReference 'call/cc) 
                                     (make-MakeCompiledProcedure call/cc-label 1 '() 'call/cc))
        ,(make-AssignPrimOpStatement (make-PrimitivesReference 'call-with-current-continuation) 
                                     (make-MakeCompiledProcedure call/cc-label 1 '() 'call/cc))
        ,(make-GotoStatement (make-Label after-call/cc-code)))
      (make-call/cc-code)
      `(,after-call/cc-code)))
   
  
   
   ;; values
   ;; values simply keeps all (but the first) value on the stack, preserves the argcount, and does a return
   ;; to the multiple-value-return address.
   (let ([after-values-body-defn (make-label 'afterValues)]
         [values-entry (make-label 'valuesEntry)]
         [on-zero-values (make-label 'onZeroValues)]
         [on-single-value (make-label 'onSingleValue)])
     `(,(make-GotoStatement (make-Label after-values-body-defn))
       ,values-entry
       ,(make-TestAndBranchStatement (make-TestOne (make-Reg 'argcount)) on-single-value)
       ,(make-TestAndBranchStatement (make-TestZero (make-Reg 'argcount)) on-zero-values)

       ;; Common case: we're running multiple values.  Put the first in the val register
       ;; and go to the multiple value return.
       ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel/MultipleValueReturn))
       ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
       ,(make-PopEnvironment (make-Const 1) (make-Const 0))
       ,(make-PopControlFrame)
       ,(make-GotoStatement (make-Reg 'proc))

       ;; Special case: on a single value, just use the regular return address
       ,on-single-value
       ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel))
       ,(make-AssignImmediateStatement 'val (make-EnvLexicalReference 0 #f))
       ,(make-PopEnvironment (make-Const 1) (make-Const 0))
       ,(make-PopControlFrame)
       ,(make-GotoStatement (make-Reg 'proc))

       ;; On zero values, leave things be and just return.
       ,on-zero-values
       ,(make-AssignImmediateStatement 'proc (make-ControlStackLabel/MultipleValueReturn))
       ,(make-PopControlFrame)
       ,(make-GotoStatement (make-Reg 'proc))
       
       ,after-values-body-defn
       ,(make-AssignPrimOpStatement (make-PrimitivesReference 'values)
                                    (make-MakeCompiledProcedure values-entry
                                                                (make-ArityAtLeast 0) 
                                                                '() 
                                                                'values))))
   
   
   
   
   ;; As is apply:
   (let ([after-apply-code (make-label 'afterApplyCode)]
         [apply-entry (make-label 'applyEntry)])
     `(,(make-GotoStatement (make-Label after-apply-code))
       ,apply-entry
       
       ;; Push the procedure into proc.
       ,(make-AssignImmediateStatement 'proc (make-EnvLexicalReference 0 #f))
       ,(make-PopEnvironment (make-Const 1) (make-Const 0))
       ;; Correct the number of arguments to be passed.
       ,(make-AssignImmediateStatement 'argcount (make-SubtractArg (make-Reg 'argcount)
                                                                   (make-Const 1)))
       ;; Splice in the list argument.
       ,(make-PerformStatement (make-SpliceListIntoStack! (make-SubtractArg (make-Reg 'argcount)
                                                                            (make-Const 1))))
       
       ;; Finally, jump into the procedure body
       ,@(statements (compile-general-procedure-call '()
                                                     (make-Reg 'argcount) ;; the stack contains only the argcount elements.
                                                     'val
                                                     return-linkage))
       
       
       ,after-apply-code
       ,(make-AssignPrimOpStatement (make-PrimitivesReference 'apply)
                                    (make-MakeCompiledProcedure apply-entry (make-ArityAtLeast 2) '() 'apply))))))