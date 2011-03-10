#lang typed/racket/base

;; An evaluator for the intermediate language, so I can do experiments.
;;
;; For example, I'll need to be able to count the number of statements executed by an evaluation.
;; I also need to do things like count pushes and pops.  Basically, low-level benchmarking.

(require "il-structs.rkt"
         "simulator-structs.rkt"
         racket/list
         racket/match
         (for-syntax racket/base))

(require/typed "simulator-prims.rkt"
               [lookup-primitive (Symbol -> PrimitiveValue)])
               

(provide new-machine can-step? step current-instruction
         
         machine-control-size)


(: new-machine ((Listof Statement) -> machine))
(define (new-machine program-text)
  (make-machine (make-undefined) (make-undefined) '() '() 0 (list->vector program-text) 0))



(: machine-control-size (machine -> Natural))
(define (machine-control-size m)
  (length (machine-control m)))




(: can-step? (machine -> Boolean))
;; Produces true if we can make a further step in the simulation.
(define (can-step? m)
  (< (machine-pc m)
     (vector-length (machine-text m))))


(: step (machine -> machine))
;; Take one simulation step.
(define (step m)
  (let: ([i : Statement (current-instruction m)])
        (increment-pc
         (cond
           [(symbol? i)
            m]
           [(AssignImmediateStatement? i)
            (step-assign-immediate m i)]
           [(AssignPrimOpStatement? i)
            (step-assign-primitive-operation m i)]
           [(PerformStatement? i)
            (step-perform m i)]
           [(GotoStatement? i)
            (step-goto m i)]
           [(TestAndBranchStatement? i)
            (step-test-and-branch m i)]
           [(PopEnvironment? i)
            (step-pop-environment m i)]
           [(PushEnvironment? i)
            (step-push-environment m i)]
           [(PushControlFrame? i)
            (step-push-control-frame m i)]
           [(PopControlFrame? i)
            (step-pop-control-frame m i)]))))

  
    
(: step-goto (machine GotoStatement -> machine))
(define (step-goto m a-goto)
  (let: ([t : (U Label Reg) (GotoStatement-target a-goto)])
        (cond [(Label? t)
               (jump m (Label-name t))]
              [(Reg? t)
               (let: ([reg : AtomicRegisterSymbol (Reg-name t)])
                     (cond [(AtomicRegisterSymbol? reg)
                            (cond [(eq? reg 'val)
                                   (jump m (ensure-symbol (machine-val m)))]
                                  [(eq? reg 'proc)
                                   (jump m (ensure-symbol (machine-proc m)))])]))])))

(: step-assign-immediate (machine AssignImmediateStatement -> machine))
(define (step-assign-immediate m stmt)
  (let: ([t : Target (AssignImmediateStatement-target stmt)]
         [v : SlotValue (evaluate-oparg m (AssignImmediateStatement-value stmt))])
        (cond [(eq? t 'proc)
               (proc-update m v)]
              [(eq? t 'val)
               (val-update m v)]
              [(EnvLexicalReference? t)
               (env-mutate m (EnvLexicalReference-depth t) v)]
              [(EnvPrefixReference? t)
               (toplevel-mutate! (ensure-toplevel (env-ref m (EnvPrefixReference-depth t)))
                                 (EnvPrefixReference-pos t)
                                 (ensure-primitive-value v))
               m])))


(: step-push-environment (machine PushEnvironment -> machine))
(define (step-push-environment m stmt)
  (let: loop : machine ([m : machine m]
                        [n : Natural (PushEnvironment-n stmt)])
    (cond
      [(= n 0)
       m]
      [else
       (loop (env-push m (make-undefined))
             (sub1 n))])))

(: step-pop-environment (machine PopEnvironment -> machine))
(define (step-pop-environment m stmt)
  (env-pop m
           (PopEnvironment-n stmt)
           (PopEnvironment-skip stmt)))


(: step-push-control-frame (machine PushControlFrame -> machine))
(define (step-push-control-frame m stmt)
  (control-push m (make-frame (PushControlFrame-label stmt)
                              (ensure-closure-or-false (machine-proc m)))))

(: step-pop-control-frame (machine PopControlFrame -> machine))
(define (step-pop-control-frame m stmt)
  (let-values: ([([m : machine]
                  [l : Symbol])
                 (control-pop m)])
               m))

(: step-test-and-branch (machine TestAndBranchStatement -> machine))
(define (step-test-and-branch m stmt)
  (let: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)]
         [argval : SlotValue (lookup-atomic-register m (TestAndBranchStatement-register stmt))])
        (if (cond
              [(eq? test 'false?)
               (not argval)]
              [(eq? test 'primitive-procedure?)
               (primitive-proc? argval)])
            (jump m (TestAndBranchStatement-label stmt))
            m)))

      
(: lookup-atomic-register (machine AtomicRegisterSymbol -> SlotValue))
(define (lookup-atomic-register m reg)
  (cond [(eq? reg 'val)
         (machine-val m)]
        [(eq? reg 'proc)
         (machine-proc m)]))


(: lookup-env-reference (machine EnvReference -> SlotValue))
(define (lookup-env-reference m ref)
  (cond [(EnvLexicalReference? ref)
         (env-ref m (EnvLexicalReference-depth ref))]
        [(EnvWholePrefixReference? ref)
         (env-ref m (EnvWholePrefixReference-depth ref))]))


(: step-perform (machine PerformStatement -> machine))
(define (step-perform m stmt)
  (let: ([op : PrimitiveCommand (PerformStatement-op stmt)])
        (cond

          [(CheckToplevelBound!? op)
           (let: ([a-top : toplevel (ensure-toplevel (env-ref m (CheckToplevelBound!-depth op)))])
                 (cond
                   [(undefined? (list-ref (toplevel-vals a-top) (CheckToplevelBound!-pos op)))
                    (error 'check-toplevel-bound! "Unbound identifier ~s" 
                           (CheckToplevelBound!-name op))]
                   [else
                    m]))]
          
          [(CheckClosureArity!? op)
           (let: ([clos : SlotValue (machine-proc m)])
                 (cond
                   [(closure? clos)
                    (if (= (closure-arity clos)
                           (CheckClosureArity!-arity op))
                        m
                        (error 'check-closure-arity "arity mismatch"))]
                   [else
                    (error 'check-closure-arity "not a closure")]))]

          [(ExtendEnvironment/Prefix!? op)
           (env-push m 
                     (make-toplevel (map lookup-primitive 
                                         (ExtendEnvironment/Prefix!-names op))))]
          
          [(InstallClosureValues!? op)
           (let: ([a-proc : SlotValue (machine-proc m)])
                 (cond
                   [(closure? a-proc)
                    (env-push-many m
                                   (closure-vals a-proc))]
                   [else
                    (error 'step-perform "Procedure register doesn't hold a procedure: ~s"
                           a-proc)]))])))

(: get-target-updater (Target -> (machine SlotValue -> machine)))
(define (get-target-updater t)
  (cond
    [(eq? t 'proc)
     proc-update]
    [(eq? t 'val)
     val-update]
    [(EnvLexicalReference? t)
     (lambda: ([m : machine] [v : SlotValue])
              (env-mutate m (EnvLexicalReference-depth t) v))]
    [(EnvPrefixReference? t)
     (lambda: ([m : machine] [v : SlotValue])
              (toplevel-mutate! (ensure-toplevel (env-ref m (EnvPrefixReference-depth t)))
                                (EnvPrefixReference-pos t)
                                (ensure-primitive-value v))
              m)]))


(: step-assign-primitive-operation (machine AssignPrimOpStatement -> machine))
(define (step-assign-primitive-operation m stmt)
  (let: ([op : PrimitiveOperator (AssignPrimOpStatement-op stmt)]
         [target-updater : (machine SlotValue -> machine)
                         (get-target-updater (AssignPrimOpStatement-target stmt))])
        (cond
          [(GetCompiledProcedureEntry? op)
           (let: ([a-proc : SlotValue (machine-proc m)])
                 (cond
                   [(closure? a-proc)
                    (target-updater m (closure-label a-proc))]
                   [else
                    (error 'get-compiled-procedure-entry)]))]

          [(MakeCompiledProcedure? op)
           (target-updater m (make-closure (MakeCompiledProcedure-label op)
                                           (MakeCompiledProcedure-arity op)
                                           (map (lambda: ([r : EnvReference])
                                                         (lookup-env-reference m r))
                                                (MakeCompiledProcedure-closed-vals op))))]

          [(ApplyPrimitiveProcedure? op)
           (let: ([prim : SlotValue (machine-proc m)]
                  [args : (Listof PrimitiveValue)
                        (map ensure-primitive-value (take (machine-env m)
                                                          (ApplyPrimitiveProcedure-arity op)))])
                 (cond
                   [(primitive-proc? prim)
                    (target-updater m (ensure-primitive-value (apply (primitive-proc-f prim) args)))]
                   [else
                    (error 'apply-primitive-procedure)]))]
          
         
          [(GetControlStackLabel? op)
           (target-updater m (frame-return (first (machine-control m))))])))

           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: evaluate-oparg (machine OpArg -> SlotValue))
(define (evaluate-oparg m an-oparg)
  (cond
    [(Const? an-oparg)
     (ensure-primitive-value (Const-const an-oparg))]
    
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
     (let: ([v : SlotValue
               (list-ref (machine-env m) (EnvLexicalReference-depth an-oparg))])
           (cond
             [(PrimitiveValue? v)
              v]
             [(toplevel? v)
              (error 'evaluate-oparg
                     "Unexpected toplevel at depth ~s"
                     (EnvLexicalReference-depth an-oparg))]))]
    
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
             [(PrimitiveValue? v)
              (error 'evaluate-oparg "Internal error: primitive value at depth ~s"
                     (EnvWholePrefixReference-depth an-oparg))]
             [(toplevel? v)
              v]))]))


(: ensure-closure-or-false (SlotValue -> (U closure #f)))
(define (ensure-closure-or-false v)
  (if (or (closure? v) (eq? v #f))
      v
      (error 'ensure-closure)))


(: ensure-primitive-value (Any -> PrimitiveValue))
;; Make sure the value is primitive.
(define (ensure-primitive-value val)
  (let: loop : PrimitiveValue ([v : Any val])
        (cond
          [(string? v)
           v]
          [(symbol? v)
           v]
          [(number? v)
           v]
          [(boolean? v)
           v]
          [(null? v)
           v]
          [(cons? v)
           (cons (loop (car v)) (loop (cdr v)))]
          [(primitive-proc? v)
           v]
          [(closure? v)
           v]
          [(undefined? v)
           v]
          [else
           (error 'ensure-primitive-value "Unable to coerse Const ~s to a primitive value" v)])))

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


(: current-instruction (machine -> Statement))
(define (current-instruction m)
  (match m
    [(struct machine (val proc env control pc text
                          stack-size))
     (vector-ref text pc)]))

  

(: val-update (machine SlotValue -> machine))
(define (val-update m v)
  (match m
    [(struct machine (val proc env control pc text
                          stack-size))
     (make-machine v proc env control pc text stack-size)]))

(: proc-update (machine SlotValue -> machine))
(define (proc-update m v)
    (match m
    [(struct machine (val proc env control pc text
                          stack-size))
     (make-machine val v env control pc text stack-size)]))

(: env-push (machine SlotValue -> machine))
(define (env-push m v)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc (cons v env) control pc text
                   (add1 stack-size))]))

(: env-push-many (machine (Listof SlotValue) -> machine))
(define (env-push-many m vs)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc (append vs env) control pc text
                   (+ stack-size (length vs)))]))


(: env-ref (machine Natural -> SlotValue))
(define (env-ref m i)  
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (list-ref env i)]))

(: env-mutate (machine Natural SlotValue -> machine))
(define (env-mutate m i v)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc (list-replace env i v) control pc text stack-size)]))

(: list-replace (All (A) (Listof A) Natural A -> (Listof A)))
(define (list-replace l i v)
  (cond
    [(= i 0)
     (cons v (rest l))]
    [else
     (cons (first l)
           (list-replace (rest l) (sub1 i) v))]))


(: env-pop (machine Natural Natural -> machine))
(define (env-pop m n skip)     
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc (append (take env skip)
                                    (drop env (+ skip n)))
                   control pc text
                   (ensure-natural (- stack-size n)))]))


(: control-push (machine frame -> machine))
(define (control-push m a-frame)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc env (cons a-frame control) pc text
                   stack-size)]))


(: control-pop (machine -> (values machine Symbol)))
(define (control-pop m)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (values (make-machine val proc env (rest control) pc text stack-size)
             (frame-return (first control)))]))

(: increment-pc (machine -> machine))
(define (increment-pc m)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc env control (add1 pc) text stack-size)]))


(: jump (machine Symbol -> machine))
;; Jumps directly to the instruction at the given label.
(define (jump m l)
  (match m
    [(struct machine (val proc env control pc text stack-size))
     (make-machine val proc env control (vector-find text l) text stack-size)]))


(: vector-find (All (A) (Vectorof A) A -> Natural))
(define (vector-find vec x)
  (let: loop : Natural ([i : Natural 0])
        (cond
          [(eq? (vector-ref vec i) x)
           i]
          [else
           (loop (add1 i))])))


(: toplevel-mutate! (toplevel Natural PrimitiveValue -> Void))
(define (toplevel-mutate! a-top index v)
  (set-toplevel-vals! a-top (append (take (toplevel-vals a-top) index)
                                    (list v)
                                    (drop (toplevel-vals a-top) (add1 index)))))
