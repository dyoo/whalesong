#lang typed/racket/base

;; An evaluator for the intermediate language, so I can do experiments.
;;
;; For example, I'll need to be able to count the number of statements executed by an evaluation.
;; I also need to do things like count pushes and pops.  Basically, low-level benchmarking.

(require "simulator-structs.rkt"
         "../compiler/expression-structs.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/bootstrapped-primitives.rkt"
         "../compiler/kernel-primitives.rkt"
         racket/list
         racket/match
         (for-syntax racket/base))

(require/typed "simulator-primitives.rkt"
               [lookup-primitive (Symbol -> PrimitiveValue)]
               [set-primitive! (Symbol PrimitiveValue -> Void)])

(require/typed "simulator-helpers.rkt"
               [ensure-primitive-value-box (SlotValue -> (Boxof PrimitiveValue))]
               [ensure-primitive-value (SlotValue -> PrimitiveValue)]
               [ensure-list (Any -> (U Null MutablePair))]
               [racket->PrimitiveValue (Any -> PrimitiveValue)])
             

(provide new-machine 
         can-step?
         step! 
         current-instruction
         current-simulated-output-port
         machine-control-size
         invoke-module-as-main)


(define current-simulated-output-port (make-parameter (current-output-port)))



(define end-of-program-text 'end-of-program-text)


(: new-machine (case-lambda [(Listof Statement) -> machine]
                            [(Listof Statement) Boolean -> machine]))
(define new-machine
  (case-lambda: 
    [([program-text : (Listof Statement)])
     (new-machine program-text #f)]
    [([program-text : (Listof Statement)]
      [with-bootstrapping-code? : Boolean])
     (let*: ([after-bootstrapping : Symbol (make-label 'afterBootstrapping)]
             [program-text : (Listof Statement)
                           (append (cond [with-bootstrapping-code?
                                          (append (get-bootstrapping-code)
                                                  program-text)]
                                         [else
                                          program-text])
                                   (list end-of-program-text))])
            (let: ([m : machine (make-machine (make-undefined)
                                              (make-undefined)
                                              (make-undefined)
                                              '() 
                                              '()
                                              0 
                                              (list->vector program-text) 
                                              ((inst make-hash Symbol module-record))
                                              0
                                              ((inst make-hash Symbol Natural)))])
                  (let: loop : Void ([i : Natural 0])
                        (when (< i (vector-length (machine-text m)))
                          (let: ([stmt : Statement (vector-ref (machine-text m) i)])
                                (when (symbol? stmt)
                                  (hash-set! (machine-jump-table m) stmt i))
                                (when (LinkedLabel? stmt)
                                  (hash-set! (machine-jump-table m) (LinkedLabel-label stmt) i))
                                (loop (add1 i)))))
                  m))]))



(: machine-control-size (machine -> Natural))
(define (machine-control-size m)
  (length (machine-control m)))





(: invoke-module-as-main (machine Symbol -> 'ok))
;; Assuming the module has been loaded in, sets the machine
;; up to invoke its body.
(define (invoke-module-as-main m module-name)
  (let ([frame (make-PromptFrame
                default-continuation-prompt-tag-value
                HALT
                (length (machine-env m))
                (make-hasheq)
                (make-hasheq))]
        [module-record (hash-ref (machine-modules m) module-name)])
    (control-push! m frame)
    (jump! m (module-record-label module-record))))
    
               



(: can-step? (machine -> Boolean))
;; Produces true if we can make a further step in the simulation.
(define (can-step? m)
  (< (machine-pc m)
     (vector-length (machine-text m))))


(: step! (machine -> 'ok))
;; Take one simulation step.
(define (step! m)
  (let*: ([i : Statement (current-instruction m)]
          [result : 'ok
                  (cond
                    [(symbol? i)
                     'ok]
                    [(LinkedLabel? i)
                     'ok]
                    [(DebugPrint? i)
                     ;; Hack to monitor evaluation.
                     (displayln (evaluate-oparg m (DebugPrint-value i)))
                     'ok]
                    [(AssignImmediateStatement? i)
                     (step-assign-immediate! m i)]
                    [(AssignPrimOpStatement? i)
                     (step-assign-primitive-operation! m i)]
                    [(PerformStatement? i)
                     (step-perform! m i)]
                    [(GotoStatement? i)
                     (step-goto! m i)]
                    [(TestAndBranchStatement? i)
                     (step-test-and-branch! m i)]
                    [(PopEnvironment? i)
                     (step-pop-environment! m i)]
                    [(PushEnvironment? i)
                     (step-push-environment! m i)]
                    [(PushImmediateOntoEnvironment? i)
                     (step-push-immediate-onto-environment! m i)]
                    [(PushControlFrame/Generic? i)
                     (step-push-control-frame/generic! m i)]
                    [(PushControlFrame/Call? i)
                     (step-push-control-frame! m i)]
                    [(PushControlFrame/Prompt? i)
                     (step-push-control-frame/prompt! m i)]
                    [(PopControlFrame? i)
                     (step-pop-control-frame! m i)]
                    [(Comment? i)
                     'ok]
                    )])
         (increment-pc! m)))




(: step-goto! (machine GotoStatement -> 'ok))
(define (step-goto! m a-goto)
  (let: ([t : Symbol (ensure-symbol (evaluate-oparg m (GotoStatement-target a-goto)))])
        (jump! m t)))
        

(: step-assign-immediate! (machine AssignImmediateStatement -> 'ok))
(define (step-assign-immediate! m stmt)
  (let: ([t : Target (AssignImmediateStatement-target stmt)]
         [v : SlotValue (evaluate-oparg m (AssignImmediateStatement-value stmt))])
        ((get-target-updater t) m v)))


(: step-push-environment! (machine PushEnvironment -> 'ok))
(define (step-push-environment! m stmt)
  (let: loop : 'ok ([n : Natural (PushEnvironment-n stmt)])
        (cond
          [(= n 0)
           'ok]
          [else
           (env-push! m (if (PushEnvironment-unbox? stmt)
                            (box (make-undefined))
                            (make-undefined)))
           (loop (sub1 n))])))

(: step-pop-environment! (machine PopEnvironment -> 'ok))
(define (step-pop-environment! m stmt)
  (env-pop! m 
            (ensure-natural (evaluate-oparg m (PopEnvironment-n stmt)))
            (ensure-natural (evaluate-oparg m (PopEnvironment-skip stmt)))))

(: step-push-immediate-onto-environment! (machine PushImmediateOntoEnvironment -> 'ok))
(define (step-push-immediate-onto-environment! m stmt)
  (let ([t (make-EnvLexicalReference 0 (PushImmediateOntoEnvironment-box? stmt))]
        [v (evaluate-oparg m (PushImmediateOntoEnvironment-value stmt))])
    (step-push-environment! m (make-PushEnvironment 1 (PushImmediateOntoEnvironment-box? stmt)))
    ((get-target-updater t) m v)))



(: step-push-control-frame/generic! (machine PushControlFrame/Generic -> 'ok))
(define (step-push-control-frame/generic! m stmt)
  (control-push! m (make-GenericFrame (make-hasheq)
                                      (make-hasheq))))


(: step-push-control-frame! (machine PushControlFrame/Call -> 'ok))
(define (step-push-control-frame! m stmt)
  (control-push! m (make-CallFrame (PushControlFrame/Call-label stmt)
                                   (ensure-closure-or-false (machine-proc m))
                                   (make-hasheq)
                                   (make-hasheq))))

(: step-push-control-frame/prompt! (machine PushControlFrame/Prompt -> 'ok))
(define (step-push-control-frame/prompt! m stmt)
  (control-push! m (make-PromptFrame
                    (let ([tag (PushControlFrame/Prompt-tag stmt)])
                      (cond 
                        [(DefaultContinuationPromptTag? tag)
                         default-continuation-prompt-tag-value]
                        [(OpArg? tag)
                         (ensure-continuation-prompt-tag-value (evaluate-oparg m tag))]))
                    (PushControlFrame/Prompt-label stmt)
                    (length (machine-env m))
                    (make-hasheq)
                    (make-hasheq))))



(: step-pop-control-frame! (machine (U PopControlFrame) -> 'ok))
(define (step-pop-control-frame! m stmt)
  (let: ([l : Symbol (control-pop! m)])
        'ok))

(: step-test-and-branch! (machine TestAndBranchStatement -> 'ok))
(define (step-test-and-branch! m stmt)
  (let: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)])
        (if (ann (cond
		  [(TestFalse? test)
		   (not (evaluate-oparg m (TestFalse-operand test)))]
		  [(TestTrue? test)
		   (and (evaluate-oparg m (TestTrue-operand test)) #t)]
		  [(TestOne? test)
		   (= (ensure-natural (evaluate-oparg m (TestOne-operand test)))
		      1)]
		  [(TestZero? test)
		   (= (ensure-natural (evaluate-oparg m (TestZero-operand test)))
		      0)]
		  [(TestPrimitiveProcedure? test)
		   (primitive-proc? (evaluate-oparg m (TestPrimitiveProcedure-operand test)))]
		  [(TestClosureArityMismatch? test)
		   (let ([proc (ensure-closure
				(evaluate-oparg m (TestClosureArityMismatch-closure test)))]
			 [n (ensure-natural 
			     (evaluate-oparg m (TestClosureArityMismatch-n test)))])
		     (not (arity-match? (closure-arity proc) n)))])
		 Boolean)
            (jump! m (TestAndBranchStatement-label stmt))
            'ok)))


(: lookup-atomic-register (machine AtomicRegisterSymbol -> SlotValue))
(define (lookup-atomic-register m reg)
  (cond [(eq? reg 'val)
         (machine-val m)]
        [(eq? reg 'proc)
         (machine-proc m)]
        [(eq? reg 'argcount)
         (machine-argcount m)]))


(: lookup-env-reference/closure-capture (machine EnvReference -> SlotValue))
;; Capture values for the closure, given a set of environment references.
(define (lookup-env-reference/closure-capture m ref)
  (cond [(EnvLexicalReference? ref)
         (if (EnvLexicalReference-unbox? ref)
             (ensure-primitive-value-box (env-ref m (EnvLexicalReference-depth ref)))
             (env-ref m (EnvLexicalReference-depth ref)))]
        [(EnvWholePrefixReference? ref)
         (env-ref m (EnvWholePrefixReference-depth ref))]))


(: step-perform! (machine PerformStatement -> 'ok))
(define (step-perform! m stmt)
  (let: ([op : PrimitiveCommand (PerformStatement-op stmt)])
        (cond
          
          [(CheckToplevelBound!? op)
           (let: ([a-top : toplevel (ensure-toplevel (env-ref m (CheckToplevelBound!-depth op)))])
                 (when (> (CheckToplevelBound!-pos op)
                          (length (toplevel-vals a-top)))
                   (printf "ERROR: toplevel is length ~s, but trying to refer to ~s.\n\n~s\n"
                           (length (toplevel-vals a-top))
                           (CheckToplevelBound!-pos op)
                           (toplevel-names a-top))
                   (for ([i (in-range (length (machine-env m)))])
                     (let ([elt (env-ref m (ensure-natural i))])
                       (when (toplevel? elt)
                         (printf "element ~s ia a toplevel of length ~s\n"
                                 i (length (toplevel-names elt))))))
                   (flush-output (current-output-port)))
                 (cond
                   [(undefined? (list-ref (toplevel-vals a-top) (CheckToplevelBound!-pos op)))
                    (error 'check-toplevel-bound! "Unbound identifier ~s" 
                           (list-ref (toplevel-names a-top) (CheckToplevelBound!-pos op)))]
                   [else
                    'ok]))]
          
          [(CheckClosureArity!? op)
           (let: ([clos : SlotValue (machine-proc m)])
                 (cond
                   [(closure? clos)
                    (if (arity-match? (closure-arity clos)
                                      (ensure-natural (evaluate-oparg m (CheckClosureArity!-arity op))))
                        'ok
                        (error 'check-closure-arity "arity mismatch: passed ~s args to ~s"
                               (ensure-natural (evaluate-oparg m (CheckClosureArity!-arity op)))
                               (closure-display-name clos)))]
                   [else
                    (error 'check-closure-arity "not a closure: ~s" clos)]))]

          [(CheckPrimitiveArity!? op)
           (let: ([clos : SlotValue (machine-proc m)])
                 (cond
                   [(primitive-proc? clos)
                    (if (arity-match? (primitive-proc-arity clos)
                                      (ensure-natural (evaluate-oparg m (CheckPrimitiveArity!-arity op))))
                        'ok
                        (error 'check-primitive-arity "arity mismatch: passed ~s args to ~s"
                               (ensure-natural (evaluate-oparg m (CheckPrimitiveArity!-arity op)))
                               (primitive-proc-display-name clos)))]
                   [else
                    (error 'check-primitive-arity "not a primitive: ~s" clos)]))]

          
          [(ExtendEnvironment/Prefix!? op)
           (env-push! m 
                      (make-toplevel
                       (ExtendEnvironment/Prefix!-names op)
                       (map (lambda: ([name : (U False Symbol GlobalBucket ModuleVariable)])
                                     (cond [(eq? name #f)
                                            (make-undefined)]
                                           [(symbol? name)
                                            (lookup-primitive name)]
                                           [(GlobalBucket? name)
                                            (lookup-primitive
                                             (GlobalBucket-name name))]
                                           [(ModuleVariable? name)
                                            (lookup-module-variable m name)]))
                            (ExtendEnvironment/Prefix!-names op))))]

          [(InstallClosureValues!? op)
           (let: ([a-proc : SlotValue (machine-proc m)])
                 (cond
                   [(closure? a-proc)
                    (env-push-many! m (closure-vals a-proc))]
                   [else
                    (error 'step-perform "Procedure register doesn't hold a procedure: ~s"
                           a-proc)]))]
          
          [(FixClosureShellMap!? op)
           (let: ([a-closure-shell : closure (ensure-closure (env-ref m (FixClosureShellMap!-depth op)))])
                 (set-closure-vals! a-closure-shell
                                    (map (lambda: ([d : Natural]) (env-ref m d))
                                         (FixClosureShellMap!-closed-vals op)))
                 'ok)]

          
          [(SetFrameCallee!? op)
           (let* ([proc-value (ensure-closure (evaluate-oparg m (SetFrameCallee!-proc op)))]
                  [frame (ensure-CallFrame (control-top m))])
             (set-CallFrame-proc! frame proc-value)
             'ok)]
          
          [(SpliceListIntoStack!? op)
           (let*: ([stack-index : Natural (ensure-natural (evaluate-oparg m (SpliceListIntoStack!-depth op)))]
                   [arg-list : (Listof PrimitiveValue) 
                             (mutable-pair-list->list
                              (ensure-list (env-ref m stack-index)))])
                  (set-machine-env! m (append (take (machine-env m) stack-index)
                                              arg-list
                                              (drop (machine-env m) (add1 stack-index))))
                  (set-machine-stack-size! m (ensure-natural (+ (machine-stack-size m)
                                                                (length arg-list)
                                                                -1)))
                  (set-machine-argcount! m
                                         (ensure-natural 
                                          (+ (ensure-natural (machine-argcount m))
                                             (length arg-list)
                                             -1)))
                  'ok)]
          
          [(UnspliceRestFromStack!? op)
           (let: ([depth : Natural (ensure-natural
                                    (evaluate-oparg m (UnspliceRestFromStack!-depth op)))]
                  [len : Natural (ensure-natural
                                  (evaluate-oparg m (UnspliceRestFromStack!-length op)))])
                 (let ([rest-arg (list->mutable-pair-list (map ensure-primitive-value
                                                               (take (drop (machine-env m) depth) len)))])
                   (set-machine-env! m 
                                     (append (take (machine-env m) depth)
                                             (list rest-arg)
                                             (drop (machine-env m) (+ depth len))))
                   (set-machine-stack-size! m (ensure-natural 
                                               (+ (machine-stack-size m)
                                                  (add1 (- len)))))
                   (set-machine-argcount! m (ensure-natural (+ (ensure-natural (machine-argcount m))
                                                               (add1 (- len)))))
                   'ok))]
          
          [(RestoreControl!? op)
           (let: ([tag-value : ContinuationPromptTagValue
                             (let ([tag (RestoreControl!-tag op)])
                               (cond
                                 [(DefaultContinuationPromptTag? tag)
                                  default-continuation-prompt-tag-value]
                                 [(OpArg? tag)
                                  (ensure-continuation-prompt-tag-value (evaluate-oparg m tag))]))])
                 (set-machine-control! m (compose-continuation-frames 
                                          (CapturedControl-frames (ensure-CapturedControl (env-ref m 0)))
                                          (drop-continuation-to-tag (machine-control m)
                                                                    tag-value)))
                 'ok)]

          [(RestoreEnvironment!? op)
           (set-machine-env! m (CapturedEnvironment-vals (ensure-CapturedEnvironment (env-ref m 1))))
           (set-machine-stack-size! m (length (machine-env m)))
           'ok]
          
          [(InstallContinuationMarkEntry!? op)
           (let* ([a-frame (control-top m)]
                  [key (hash-ref (frame-temps a-frame) 'pendingContinuationMarkKey)]
                  [val (machine-val m)]
                  [marks (frame-marks a-frame)])
             (hash-set! marks 
                        (ensure-primitive-value key)
                        (ensure-primitive-value val))
             'ok)]
          
          [(RaiseContextExpectedValuesError!? op)
           (error 'step "context expected ~a values, received ~a values."
                  (RaiseContextExpectedValuesError!-expected op)
                  (machine-argcount m))]

	  [(RaiseArityMismatchError!? op)
	   (error 'step "expects ~s arguments, given ~a"
		  (RaiseArityMismatchError!-expected op)
		  (evaluate-oparg m (RaiseArityMismatchError!-received op)))]

	  [(RaiseOperatorApplicationError!? op)
	   (error 'step "expected procedure, given ~a"
		  (evaluate-oparg m (RaiseOperatorApplicationError!-operator op)))]
          
	  [(RaiseUnimplementedPrimitiveError!? op)
	   (error 'step "Unimplemented kernel procedure ~a"
		  (RaiseUnimplementedPrimitiveError!-name op))]

          
          [(InstallModuleEntry!? op)
           (hash-set! (machine-modules m)
                      (ModuleLocator-name (InstallModuleEntry!-path op))
                      (make-module-record (InstallModuleEntry!-name op)
                                          (ModuleLocator-name 
                                           (InstallModuleEntry!-path op))
                                          (InstallModuleEntry!-entry-point op)
                                          #f
                                          (make-hash)
                                          #f))
           'ok]


          [(MarkModuleInvoked!? op)
           (let ([module-record
                  (hash-ref (machine-modules m)
                            (ModuleLocator-name (MarkModuleInvoked!-path op)))])
             (set-module-record-invoked?! module-record #t)
           'ok)]
          [(AliasModuleAsMain!? op)
           (let ([module-record
                  (hash-ref (machine-modules m)
                            (ModuleLocator-name (AliasModuleAsMain!-from op)))])
             (hash-set! (machine-modules m)
                        '*main*
                        module-record)
             'ok)]
          [(FinalizeModuleInvokation!? op)
           (let* ([mrecord
                   (hash-ref (machine-modules m)
                             (ModuleLocator-name (FinalizeModuleInvokation!-path op)))]
                  [ns (module-record-namespace mrecord)]
                  [top
                   (module-record-toplevel mrecord)])
             (cond
              [(toplevel? top)
               (for-each (lambda: ([n : (U False Symbol GlobalBucket ModuleVariable)]
                                   [v : PrimitiveValue])
                           (cond
                            [(eq? n #f)
                             (void)]
                            [(symbol? n)
                             (hash-set! ns n v)]
                            [(GlobalBucket? n)
                             (hash-set! ns (GlobalBucket-name n) v)]
                            [(ModuleVariable? n)
                             (hash-set! ns (ModuleVariable-name n) v)]))
                         (toplevel-names top)
                         (toplevel-vals top))
               'ok]
              [(eq? top #f)
               ;; This should never happen.  But let's make sure we can see the
               ;; error.
               (error 'FinalizeModuleInvokation
                      "internal error: toplevel hasn't been initialized.")]))])))


(: lookup-module-variable (machine ModuleVariable -> PrimitiveValue))
(define (lookup-module-variable m mv)
  (cond
   [(or (eq?
         (ModuleLocator-name
          (ModuleVariable-module-name mv))
         '#%kernel)
        (eq?
         (ModuleLocator-name
          (ModuleVariable-module-name mv))
         'whalesong/lang/kernel.rkt))
    (lookup-primitive (ModuleVariable-name mv))]
   [else
    
    (let ([mrecord
           (hash-ref (machine-modules m)
                     (ModuleLocator-name (ModuleVariable-module-name mv)))])
      (hash-ref (module-record-namespace mrecord)
                (ModuleVariable-name mv)))]))
   
  


(: mutable-pair-list->list ((U Null MutablePair) -> (Listof PrimitiveValue)))
(define (mutable-pair-list->list mlst)
  (cond
    [(null? mlst)
     '()]
    [else
     (cons (MutablePair-h mlst)
           (mutable-pair-list->list (let ([t (MutablePair-t mlst)])
                                      (cond
                                        [(null? t)
                                         t]
                                        [(MutablePair? t)
                                         t]
                                        [else
                                         (error 'mutable-pair-list->list "Not a list: ~s" t)]))))]))


(: arity-match? (Arity Natural -> Boolean))
(define (arity-match? an-arity n)
  (cond
    [(natural? an-arity)
     (= n an-arity)]
    [(ArityAtLeast? an-arity)
     (>= n (ArityAtLeast-value an-arity))]
    [(list? an-arity)
     (ormap (lambda: ([atomic-arity : (U Natural ArityAtLeast)])
                     (cond [(natural? atomic-arity)
                            (= n atomic-arity)]
                           [(ArityAtLeast? atomic-arity)
                            (>= n (ArityAtLeast-value atomic-arity))]))
            an-arity)]))
                     



(: compose-continuation-frames ((Listof frame) (Listof frame) -> (Listof frame)))
;; Stitch together the continuation.  A PromptFrame must exist at the head of frames-2.
(define (compose-continuation-frames frames-1 frames-2)
  (append frames-1 frames-2))





(: get-target-updater (Target -> (machine SlotValue -> 'ok)))
(define (get-target-updater t)
  (cond
    [(eq? t 'proc)
     proc-update!]
    [(eq? t 'val)
     val-update!]
    [(eq? t 'argcount)
     argcount-update!]
    [(EnvLexicalReference? t)
     (lambda: ([m : machine] [v : SlotValue])
              (if (EnvLexicalReference-unbox? t)
                  (begin
                    (set-box! (ensure-primitive-value-box (env-ref m (EnvLexicalReference-depth t))) 
                              (ensure-primitive-value v))
                    'ok)
                  (env-mutate! m (EnvLexicalReference-depth t) v)))]
    [(EnvPrefixReference? t)
     (lambda: ([m : machine] [v : SlotValue])
              (toplevel-mutate! (ensure-toplevel (env-ref m (EnvPrefixReference-depth t)))
                                (EnvPrefixReference-pos t)
                                (ensure-primitive-value v)))]
    [(PrimitivesReference? t)
     (lambda: ([m : machine] [v : SlotValue])
              (set-primitive! (PrimitivesReference-name t)
                              (ensure-primitive-value v))
              'ok)]
    [(ControlFrameTemporary? t)
     (lambda: ([m : machine] [v : SlotValue])
              (let ([ht (frame-temps (control-top m))])
                (hash-set! ht
                           (ControlFrameTemporary-name t)
                           (ensure-primitive-value v))
                'ok))]
    [(ModulePrefixTarget? t)
     (lambda: ([m : machine] [v : SlotValue])
              (let ([module-record
                     (hash-ref (machine-modules m)
                               (ModuleLocator-name
                                (ModulePrefixTarget-path t)))])
                (set-module-record-toplevel! module-record
                                             (ensure-toplevel v))
                'ok))]))



(: step-assign-primitive-operation! (machine AssignPrimOpStatement -> 'ok))
(define (step-assign-primitive-operation! m stmt)
  (let: ([op : PrimitiveOperator (AssignPrimOpStatement-op stmt)]
         [target-updater! : (machine SlotValue -> 'ok)
                          (get-target-updater (AssignPrimOpStatement-target stmt))])
        (cond
          [(GetCompiledProcedureEntry? op)
           (let: ([a-proc : SlotValue (machine-proc m)])
                 (cond
                   [(closure? a-proc)
                    (target-updater! m (closure-label a-proc))]
                   [else
                    (error 'get-compiled-procedure-entry)]))]
          
          [(MakeCompiledProcedure? op)
           (target-updater! m (make-closure (MakeCompiledProcedure-label op)
                                            (MakeCompiledProcedure-arity op)
                                            (map (lambda: ([d : Natural]) (env-ref m d))
                                                 (MakeCompiledProcedure-closed-vals op))
                                            (MakeCompiledProcedure-display-name op)))]

          [(MakeCompiledProcedureShell? op)
           (target-updater! m (make-closure (MakeCompiledProcedureShell-label op)
                                            (MakeCompiledProcedureShell-arity op)
                                            '()
                                            (MakeCompiledProcedureShell-display-name op)))]
                            
          [(ApplyPrimitiveProcedure? op)
           (let: ([prim : SlotValue (machine-proc m)]
                  [args : (Listof PrimitiveValue)
                        (map ensure-primitive-value (take (machine-env m)
                                                          (ensure-natural (machine-argcount m))))])
                 (cond
                   [(primitive-proc? prim)
                    (target-updater! m (ensure-primitive-value 
                                        (parameterize ([current-output-port
                                                        (current-simulated-output-port)])
                                          (apply (primitive-proc-f prim)
                                                 m
                                                 args))))]
                   [else
                    (error 'apply-primitive-procedure)]))]
          
          [(CaptureEnvironment? op)
           (target-updater! m (make-CapturedEnvironment (drop (machine-env m)
                                                              (CaptureEnvironment-skip op))))]
          [(CaptureControl? op)
           (target-updater! m (evaluate-continuation-capture m op))]
          
          [(MakeBoxedEnvironmentValue? op)
           (target-updater! m (box (ensure-primitive-value
                                    (env-ref m (MakeBoxedEnvironmentValue-depth op)))))]
          
          [(CallKernelPrimitiveProcedure? op)
           (target-updater! m (evaluate-kernel-primitive-procedure-call m op))])))


(: evaluate-continuation-capture (machine CaptureControl -> SlotValue))
(define (evaluate-continuation-capture m op)
  (let: ([frames : (Listof frame) (drop (machine-control m)
                                        (CaptureControl-skip op))]
         [tag : ContinuationPromptTagValue
              (let ([tag (CaptureControl-tag op)])
                (cond
                  [(DefaultContinuationPromptTag? tag)
                   default-continuation-prompt-tag-value]
                  [(OpArg? tag)
                   (ensure-continuation-prompt-tag-value (evaluate-oparg m tag))]))])
        (make-CapturedControl (take-continuation-to-tag frames tag))))


(: take-continuation-to-tag ((Listof frame) ContinuationPromptTagValue -> (Listof frame)))
(define (take-continuation-to-tag frames tag)
  (cond
    [(empty? frames)
     (error 'trim-continuation-at-tag "Unable to find continuation tag value ~s" tag)]
    [else
     (let ([a-frame (first frames)])
       (cond
         [(GenericFrame? a-frame)
          (cons a-frame (take-continuation-to-tag (rest frames) tag))]
         [(CallFrame? a-frame)
          (cons a-frame (take-continuation-to-tag (rest frames) tag))]
         [(PromptFrame? a-frame)
          (cond
            [(eq? (PromptFrame-tag a-frame) tag)
             '()]
            [else
             (cons a-frame (take-continuation-to-tag (rest frames) tag))])]))]))


(: drop-continuation-to-tag ((Listof frame) ContinuationPromptTagValue -> (Listof frame)))
;; Drops continuation frames until we reach the appropriate one.
(define (drop-continuation-to-tag frames tag)
  (cond
    [(empty? frames)
     (error 'trim-continuation-at-tag "Unable to find continuation tag value ~s" tag)]
    [else
     (let ([a-frame (first frames)])
       (cond
         [(GenericFrame? a-frame)
          (drop-continuation-to-tag (rest frames) tag)]
         [(CallFrame? a-frame)
          (drop-continuation-to-tag (rest frames) tag)]
         [(PromptFrame? a-frame)
          (cond
            [(eq? (PromptFrame-tag a-frame) tag)
             frames]
            [else
             (drop-continuation-to-tag (rest frames) tag)])]))]))


(: list->mutable-pair-list ((Listof PrimitiveValue) -> PrimitiveValue))
(define (list->mutable-pair-list rand-vals)
  (let: loop : PrimitiveValue ([rand-vals : (Listof PrimitiveValue) rand-vals])
             (cond [(empty? rand-vals)
                    null]
                   [else
                    (make-MutablePair (first rand-vals)
                                      (loop (rest rand-vals)))])))



(: evaluate-kernel-primitive-procedure-call (machine CallKernelPrimitiveProcedure -> PrimitiveValue))
(define (evaluate-kernel-primitive-procedure-call m op)
  (let: ([op : KernelPrimitiveName (CallKernelPrimitiveProcedure-operator op)]
         [rand-vals : (Listof PrimitiveValue)
                    (map (lambda: ([a : OpArg])
                                  (ensure-primitive-value (evaluate-oparg m a)))
                         (CallKernelPrimitiveProcedure-operands op))])
        (case op
          [(+)
           (apply + (map ensure-number rand-vals))]
          [(-)
           (apply - (ensure-number (first rand-vals)) (map ensure-number (rest rand-vals)))]
          [(*)
           (apply * (map ensure-number rand-vals))]
          [(/)
           (apply / (ensure-number (first rand-vals)) (map ensure-number (rest rand-vals)))]
          [(add1)
           (add1 (ensure-number (first rand-vals)))]
          [(sub1)
           (sub1 (ensure-number (first rand-vals)))]
          [(<)
           (chain-compare < (map ensure-real-number rand-vals))]
          [(<=)   
           (chain-compare <= (map ensure-real-number rand-vals))]
          [(=)
           (chain-compare = (map ensure-real-number rand-vals))]
          [(>)
           (chain-compare > (map ensure-real-number rand-vals))]
          [(>=)   
           (chain-compare >= (map ensure-real-number rand-vals))]
          [(cons)
           (make-MutablePair (first rand-vals) (second rand-vals))]
          [(car)
           (MutablePair-h (ensure-mutable-pair (first rand-vals)))]
          [(cdr)
           (MutablePair-t (ensure-mutable-pair (first rand-vals)))]
          [(list)
           (list->mutable-pair-list rand-vals)]
          [(null?)
           (null? (first rand-vals))]
          [(not)
           (not (first rand-vals))]
          [(eq?)
           (eq? (first rand-vals) (second rand-vals))]
          [else
           (error 'evaluate-kernel-primitive-procedure-call "missing operator: ~s\n" op)])))

(: chain-compare (All (A) (A A -> Boolean) (Listof A) -> Boolean))
(define (chain-compare f vals)
  (cond
    [(empty? vals)
     #t]
    [(empty? (rest vals))
     #t]
    [else
     (and (f (first vals) (second vals))
          (chain-compare f (rest vals)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: evaluate-oparg (machine OpArg -> SlotValue))
(define (evaluate-oparg m an-oparg)
  (cond
    [(Const? an-oparg)
     (racket->PrimitiveValue (Const-const an-oparg))]
    
    [(Label? an-oparg)
     (Label-name an-oparg)]
    
    [(Reg? an-oparg)
     (let: ([n : AtomicRegisterSymbol (Reg-name an-oparg)])
           (cond
             [(eq? n 'proc)
              (machine-proc m)]
             [(eq? n 'val)
              (machine-val m)]
             [(eq? n 'argcount)
              (machine-argcount m)]))]
    
    [(EnvLexicalReference? an-oparg)
     (let*: ([v : SlotValue
                (env-ref m (EnvLexicalReference-depth an-oparg))]
             [v : SlotValue
                (if (EnvLexicalReference-unbox? an-oparg)
                    (unbox (ensure-primitive-value-box v))
                    v)])
           (cond
             [(toplevel? v)
              (error 'evaluate-oparg
                     "Unexpected toplevel at depth ~s"
                     (EnvLexicalReference-depth an-oparg))]
             [else v]))]
    
    [(EnvPrefixReference? an-oparg)
     (let: ([a-top : SlotValue (env-ref m (EnvPrefixReference-depth an-oparg))])
           (cond
             [(toplevel? a-top)
              (list-ref (toplevel-vals a-top)
                        (EnvPrefixReference-pos an-oparg))]
             [else
              (error 'evaluate-oparg "not a toplevel: ~s" a-top)]))]
    
    
    [(EnvWholePrefixReference? an-oparg)
     (let: ([v : SlotValue
               (list-ref (machine-env m) (EnvWholePrefixReference-depth an-oparg))])
           (cond
             [(toplevel? v)
              v]
             [else
              (error 'evaluate-oparg "Internal error: not a toplevel at depth ~s: ~s"
                     (EnvWholePrefixReference-depth an-oparg)
                     v)]))]

    [(SubtractArg? an-oparg)
     (- (ensure-number (evaluate-oparg m (SubtractArg-lhs an-oparg)))
        (ensure-number (evaluate-oparg m (SubtractArg-rhs an-oparg))))]



    [(ControlStackLabel? an-oparg)
     (let ([frame (ensure-frame (first (machine-control m)))])
       (cond
	[(GenericFrame? frame)
	 (error 'GetControlStackLabel)]
	[(PromptFrame? frame)
	 (let ([label (PromptFrame-return frame)])
	   (cond [(halt? label)
                  end-of-program-text]
                 [else
                  (LinkedLabel-label label)]))]
	[(CallFrame? frame)
	 (let ([label (CallFrame-return frame)])
	   (cond
             [(halt? label)
              end-of-program-text]
             [else
              (LinkedLabel-label label)]))]))]
    
    [(ControlStackLabel/MultipleValueReturn? an-oparg)
     (let ([frame (ensure-frame (first (machine-control m)))])
       (cond
	[(GenericFrame? frame)
	 (error 'GetControlStackLabel/MultipleValueReturn)]
	[(PromptFrame? frame)
	 (let ([label (PromptFrame-return frame)])
	   (cond
             [(halt? label)
              end-of-program-text]
             [else
              (LinkedLabel-linked-to label)]))]
	[(CallFrame? frame)
	 (let ([label (CallFrame-return frame)])
           (cond
             [(halt? label)
              end-of-program-text]
             [else
              (LinkedLabel-linked-to label)]))]))]
    
    [(ControlFrameTemporary? an-oparg)
     (let ([ht (frame-temps (control-top m))])
       (hash-ref ht
                 (ControlFrameTemporary-name an-oparg)))]
    
    [(CompiledProcedureEntry? an-oparg)
     (let ([proc (ensure-closure (evaluate-oparg m (CompiledProcedureEntry-proc an-oparg)))])
       (closure-label proc))]
    
    [(CompiledProcedureClosureReference? an-oparg)
     (let ([proc (ensure-closure (evaluate-oparg m (CompiledProcedureClosureReference-proc an-oparg)))])
       (list-ref (closure-vals proc) (CompiledProcedureClosureReference-n an-oparg)))]
    
    [(PrimitiveKernelValue? an-oparg)
     (lookup-primitive (PrimitiveKernelValue-id an-oparg))]
    
    [(ModuleEntry? an-oparg)
     (let ([a-module (hash-ref (machine-modules m) 
                               (ModuleLocator-name (ModuleEntry-name an-oparg)))])
       (module-record-label a-module))]

    [(IsModuleInvoked? an-oparg)
     (let ([a-module (hash-ref (machine-modules m) 
                               (ModuleLocator-name (IsModuleInvoked-name an-oparg)))])
       (module-record-invoked? a-module))]

    [(IsModuleLinked? an-oparg)
     (hash-has-key? (machine-modules m) 
		    (ModuleLocator-name (IsModuleLinked-name an-oparg)))]

    [(VariableReference? an-oparg)
     (let ([t (VariableReference-toplevel an-oparg)])
       (make-ToplevelReference (ensure-toplevel (env-ref m (ToplevelRef-depth t)))
                               (ToplevelRef-pos t)))]))
       



(: ensure-closure-or-false (SlotValue -> (U closure #f)))
(define (ensure-closure-or-false v)
  (if (or (closure? v) (eq? v #f))
      v
      (error 'ensure-closure-or-false)))

(: ensure-closure (SlotValue -> closure))
(define (ensure-closure v)
  (if (closure? v)
      v
      (error 'ensure-closure)))

(: ensure-CallFrame (Any -> CallFrame))
(define (ensure-CallFrame v)
  (if (CallFrame? v)
      v
      (error 'ensure-CallFrame "not a CallFrame: ~s" v)))

(: ensure-continuation-prompt-tag-value (Any -> ContinuationPromptTagValue))
(define (ensure-continuation-prompt-tag-value v)
  (if (ContinuationPromptTagValue? v)
      v
      (error 'ensure-ContinuationPromptTagValue "not a ContinuationPromptTagValue: ~s" v)))


(: ensure-symbol (Any -> Symbol))
;; Make sure the value is a symbol.
(define (ensure-symbol v)
  (cond
    [(symbol? v)
     v]
    [else
     (error 'ensure-symbol)]))


(: ensure-toplevel (Any -> toplevel))
(define (ensure-toplevel v)
  (cond
    [(toplevel? v)
     v]
    [else
     (error 'ensure-toplevel)]))

(define-predicate natural? Natural)

(: ensure-natural (Any -> Natural))
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural "not a natural: ~s" x)))

(: ensure-number (Any -> Number))
(define (ensure-number x)
  (if (number? x)
      x
      (error 'ensure-number "Not a number: ~s" x)))



(: ensure-real-number (Any -> Real))
(define (ensure-real-number x)
  (if (real? x)
      x
      (error 'ensure-number "Not a number: ~s" x)))


(: ensure-mutable-pair (Any -> MutablePair))
(define (ensure-mutable-pair x)
  (if (MutablePair? x)
      x
      (error 'ensure-mutable-pair "not a mutable pair: ~s" x)))

(: ensure-prompt-frame (Any -> PromptFrame))
(define (ensure-prompt-frame x)
  (if (PromptFrame? x)
      x
      (error 'ensure-prompt-frame "not a PromptFrame: ~s" x)))

(: ensure-frame (Any -> frame))
(define (ensure-frame x)
  (if (frame? x)
      x
      (error 'ensure-frame "not a frame: ~s" x)))


(: ensure-CapturedControl (Any -> CapturedControl))
(define (ensure-CapturedControl x)
  (if (CapturedControl? x)
      x
      (error 'ensure-CapturedControl "~s" x)))




(: ensure-CapturedEnvironment (Any -> CapturedEnvironment))
(define (ensure-CapturedEnvironment x)
  (if (CapturedEnvironment? x)
      x
      (error 'ensure-CapturedEnvironment "~s" x)))


(: current-instruction (machine -> Statement))
(define (current-instruction m)
  (match m
    [(struct machine (val proc argcount env control pc text
                          modules stack-size jump-table))
     (vector-ref text pc)]))



(: val-update! (machine SlotValue -> 'ok))
(define (val-update! m v)
  (set-machine-val! m v)
  'ok)

(: argcount-update! (machine SlotValue -> 'ok))
(define (argcount-update! m v)
  (set-machine-argcount! m v)
  'ok)

(: proc-update! (machine SlotValue -> 'ok))
(define (proc-update! m v)
  (set-machine-proc! m v)
  'ok)


(: env-push! (machine SlotValue -> 'ok))
(define (env-push! m v)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-env! m (cons v env))
     (set-machine-stack-size! m (add1 stack-size))
     'ok]))

(: env-push-many! (machine (Listof SlotValue) -> 'ok))
(define (env-push-many! m vs)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-env! m (append vs env))
     (set-machine-stack-size! m (+ stack-size (length vs)))
     'ok]))


(: env-ref (machine Natural -> SlotValue))
(define (env-ref m i)  
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (list-ref env i)]))

(: env-mutate! (machine Natural SlotValue -> 'ok))
(define (env-mutate! m i v)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-env! m (list-replace env i v))
     'ok]))


(: list-replace (All (A) (Listof A) Natural A -> (Listof A)))
(define (list-replace l i v)
  (cond
    [(= i 0)
     (cons v (rest l))]
    [else
     (cons (first l)
           (list-replace (rest l) (sub1 i) v))]))


(: env-pop! (machine Natural Natural -> 'ok))
(define (env-pop! m n skip)     
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-env! m (append (take env skip)
                                 (drop env (+ skip n))))
     (set-machine-stack-size! m (ensure-natural (- stack-size n)))
     'ok]))


(: control-push! (machine frame -> 'ok))
(define (control-push! m a-frame)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-control! m (cons a-frame control))
     'ok]))


(: control-pop! (machine -> 'ok))
(define (control-pop! m)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-control! m (rest control))
     'ok]))

(: control-top (machine -> frame))
(define (control-top m)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (first control)]))



(: increment-pc! (machine -> 'ok))
(define (increment-pc! m)
  (set-machine-pc! m (add1 (machine-pc m)))
  'ok)



(: jump! (machine Symbol -> 'ok))
;; Jumps directly to the instruction at the given label.
(define (jump! m l)
  (match m
    [(struct machine (val proc argcount env control pc text modules stack-size jump-table))
     (set-machine-pc! m (hash-ref jump-table l))
     'ok]))




(: toplevel-mutate! (toplevel Natural PrimitiveValue -> 'ok))
(define (toplevel-mutate! a-top index v)
  (set-toplevel-vals! a-top (append (take (toplevel-vals a-top) index)
                                    (list v)
                                    (drop (toplevel-vals a-top) (add1 index))))
  'ok)
