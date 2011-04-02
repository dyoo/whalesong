#lang typed/racket/base

;; An evaluator for the intermediate language, so I can do experiments.
;;
;; For example, I'll need to be able to count the number of statements executed by an evaluation.
;; I also need to do things like count pushes and pops.  Basically, low-level benchmarking.

(require "il-structs.rkt"
         "lexical-structs.rkt"
         "simulator-structs.rkt"
         "bootstrapped-primitives.rkt"
         "kernel-primitives.rkt"
         racket/list
         racket/match
         (for-syntax racket/base))

(require/typed "simulator-primitives.rkt"
               [lookup-primitive (Symbol -> PrimitiveValue)]
               [set-primitive! (Symbol PrimitiveValue -> Void)])

(require/typed "simulator-helpers.rkt"
               [ensure-primitive-value-box (SlotValue -> (Boxof PrimitiveValue))]
               [ensure-primitive-value (SlotValue -> PrimitiveValue)]
               [ensure-list (Any -> PrimitiveValue)]
               [racket->PrimitiveValue (Any -> PrimitiveValue)])
             

(provide new-machine can-step? step! current-instruction
         current-simulated-output-port
         machine-control-size)


(define current-simulated-output-port (make-parameter (current-output-port)))


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
                           (cond [with-bootstrapping-code?
                                  (append (get-bootstrapping-code)
                                          program-text)]
                                 [else
                                  program-text])])
            (let: ([m : machine (make-machine (make-undefined)
                                              (make-undefined)
                                              '() 
                                              '()
                                              0 
                                              (list->vector program-text) 
                                              0
                                              ((inst make-hash Symbol Natural)))])
                  (let: loop : Void ([i : Natural 0])
                        (when (< i (vector-length (machine-text m)))
                          (let: ([stmt : Statement (vector-ref (machine-text m) i)])
                                (when (symbol? stmt)
                                  (hash-set! (machine-jump-table m) stmt i))
                                (loop (add1 i)))))
                  m))]))



(: machine-control-size (machine -> Natural))
(define (machine-control-size m)
  (length (machine-control m)))




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
                    [(PushControlFrame? i)
                     (step-push-control-frame! m i)]
                    [(PushControlFrame/Prompt? i)
                     (step-push-control-frame/prompt! m i)]
                    [(PopControlFrame? i)
                     (step-pop-control-frame! m i)]
                    [(PopControlFrame/Prompt? i)
                     (step-pop-control-frame! m i)])])
         (increment-pc! m)))




(: step-goto! (machine GotoStatement -> 'ok))
(define (step-goto! m a-goto)
  (let: ([t : (U Label Reg) (GotoStatement-target a-goto)])
        (cond [(Label? t)
               (jump! m (Label-name t))]
              [(Reg? t)
               (let: ([reg : AtomicRegisterSymbol (Reg-name t)])
                     (cond [(AtomicRegisterSymbol? reg)
                            (cond [(eq? reg 'val)
                                   (jump! m (ensure-symbol (machine-val m)))]
                                  [(eq? reg 'proc)
                                   (jump! m (ensure-symbol (machine-proc m)))])]))])))

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
  (env-pop! m (PopEnvironment-n stmt) (PopEnvironment-skip stmt)))


(: step-push-control-frame! (machine PushControlFrame -> 'ok))
(define (step-push-control-frame! m stmt)
  (control-push! m (make-CallFrame (PushControlFrame-label stmt)
                                   (ensure-closure-or-false (machine-proc m)))))

(: step-push-control-frame/prompt! (machine PushControlFrame/Prompt -> 'ok))
(define (step-push-control-frame/prompt! m stmt)
  (control-push! m (make-PromptFrame
                    (let ([tag (PushControlFrame/Prompt-tag stmt)])
                      (cond 
                        [(DefaultContinuationPromptTag? tag)
                         default-continuation-prompt-tag-value]
                        [(OpArg? tag)
                         (ensure-continuation-prompt-tag-value (evaluate-oparg m tag))]))
                    (PushControlFrame/Prompt-label stmt))))
                        
  

(: step-pop-control-frame! (machine (U PopControlFrame PopControlFrame/Prompt) -> 'ok))
(define (step-pop-control-frame! m stmt)
  (let: ([l : Symbol (control-pop! m)])
        'ok))

(: step-test-and-branch! (machine TestAndBranchStatement -> 'ok))
(define (step-test-and-branch! m stmt)
  (let: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)]
         [argval : SlotValue (lookup-atomic-register m (TestAndBranchStatement-register stmt))])
        (if (cond
              [(eq? test 'false?)
               (not argval)]
              [(eq? test 'primitive-procedure?)
               (primitive-proc? argval)])
            (jump! m (TestAndBranchStatement-label stmt))
            'ok)))


(: lookup-atomic-register (machine AtomicRegisterSymbol -> SlotValue))
(define (lookup-atomic-register m reg)
  (cond [(eq? reg 'val)
         (machine-val m)]
        [(eq? reg 'proc)
         (machine-proc m)]))


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
                    (if (= (closure-arity clos)
                           (CheckClosureArity!-arity op))
                        'ok
                        (error 'check-closure-arity "arity mismatch: passed ~s args to ~s"
                               (CheckClosureArity!-arity op)
                               (closure-display-name clos)))]
                   [else
                    (error 'check-closure-arity "not a closure: ~s" clos)]))]
          
          [(ExtendEnvironment/Prefix!? op)
           (env-push! m 
                      (make-toplevel (ExtendEnvironment/Prefix!-names op)
                                     (map (lambda: ([name : (U Symbol ModuleVariable False)])
                                                   (cond [(symbol? name)
                                                          (lookup-primitive name)]
                                                         [(ModuleVariable? name)
                                                          (lookup-primitive (ModuleVariable-name name))]
                                                         [(eq? name #f)
                                                          (make-undefined)]))
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
           'ok])))



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
              'ok)]))


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
                                                          (ApplyPrimitiveProcedure-arity op)))])
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
          
          [(GetControlStackLabel? op)
           (target-updater! m (let ([frame (ensure-frame (first (machine-control m)))])
                                (cond
                                  [(PromptFrame? frame)
                                   (PromptFrame-return frame)]
                                  [(CallFrame? frame)
                                   (CallFrame-return frame)])))]

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
         [(CallFrame? a-frame)
          (drop-continuation-to-tag (rest frames) tag)]
         [(PromptFrame? a-frame)
          (cond
            [(eq? (PromptFrame-tag a-frame) tag)
             frames]
            [else
             (drop-continuation-to-tag (rest frames) tag)])]))]))






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
           (let: loop : PrimitiveValue ([rand-vals : (Listof PrimitiveValue) rand-vals])
             (cond [(empty? rand-vals)
                    null]
                   [else
                    (make-MutablePair (first rand-vals)
                                      (loop (rest rand-vals)))]))]
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
              (machine-val m)]))]
    
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
                     v)]))]))


(: ensure-closure-or-false (SlotValue -> (U closure #f)))
(define (ensure-closure-or-false v)
  (if (or (closure? v) (eq? v #f))
      v
      (error 'ensure-closure)))

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

(: ensure-natural (Integer -> Natural))
(define (ensure-natural x)
  (if (>= x 0)
      x
      (error 'ensure-natural)))

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
    [(struct machine (val proc env control pc text
                          stack-size jump-table))
     (vector-ref text pc)]))



(: val-update! (machine SlotValue -> 'ok))
(define (val-update! m v)
  (set-machine-val! m v)
  'ok)


(: proc-update! (machine SlotValue -> 'ok))
(define (proc-update! m v)
  (set-machine-proc! m v)
  'ok)


(: env-push! (machine SlotValue -> 'ok))
(define (env-push! m v)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-env! m (cons v env))
     (set-machine-stack-size! m (add1 stack-size))
     'ok]))

(: env-push-many! (machine (Listof SlotValue) -> 'ok))
(define (env-push-many! m vs)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-env! m (append vs env))
     (set-machine-stack-size! m (+ stack-size (length vs)))
     'ok]))


(: env-ref (machine Natural -> SlotValue))
(define (env-ref m i)  
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (list-ref env i)]))

(: env-mutate! (machine Natural SlotValue -> 'ok))
(define (env-mutate! m i v)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
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
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-env! m (append (take env skip)
                                 (drop env (+ skip n))))
     (set-machine-stack-size! m (ensure-natural (- stack-size n)))
     'ok]))


(: control-push! (machine frame -> 'ok))
(define (control-push! m a-frame)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-control! m (cons a-frame control))
     'ok]))


(: control-pop! (machine -> 'ok))
(define (control-pop! m)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-control! m (rest control))
     'ok]))



(: increment-pc! (machine -> 'ok))
(define (increment-pc! m)
  (set-machine-pc! m (add1 (machine-pc m)))
  'ok)



(: jump! (machine Symbol -> 'ok))
;; Jumps directly to the instruction at the given label.
(define (jump! m l)
  (match m
    [(struct machine (val proc env control pc text stack-size jump-table))
     (set-machine-pc! m (hash-ref jump-table l))
     'ok]))




(: toplevel-mutate! (toplevel Natural PrimitiveValue -> 'ok))
(define (toplevel-mutate! a-top index v)
  (set-toplevel-vals! a-top (append (take (toplevel-vals a-top) index)
                                    (list v)
                                    (drop (toplevel-vals a-top) (add1 index))))
  'ok)
