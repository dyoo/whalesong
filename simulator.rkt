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
               [lookup-primitive (Symbol -> Any)])
               

(provide new-machine can-step? step)


(: new-machine ((Listof Statement) -> machine))
(define (new-machine program-text)
  (make-machine (make-undefined) (make-undefined) '() '() 0 (list->vector program-text)))


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
            (error 'step)]
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
               (env-mutate m (EnvLexicalReference-depth t) v)])))


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
  (control-push m (PushControlFrame-label stmt)))

(: step-pop-control-frame (machine PopControlFrame -> machine))
(define (step-pop-control-frame m stmt)
  (let-values: ([([m : machine]
                  [l : Symbol])
                 (control-pop m)])
               m))

(: step-test-and-branch (machine TestAndBranchStatement -> machine))
(define (step-test-and-branch m stmt)
  (let: ([test : PrimitiveTest (TestAndBranchStatement-op stmt)]
         [argval : Any (lookup-atomic-register m (TestAndBranchStatement-register stmt))])
        (if (cond
              [(eq? test 'false?)
               (not argval)]
              [(eq? test 'primitive-procedure?)
               (primitive-proc? argval)])
            (jump m (TestAndBranchStatement-label stmt))
            m)))

      
(: lookup-atomic-register (machine AtomicRegisterSymbol -> Any))
(define (lookup-atomic-register m reg)
  (cond [(eq? reg 'val)
         (machine-val m)]
        [(eq? reg 'proc)
         (machine-proc m)]))


(: step-perform (machine PerformStatement -> machine))
(define (step-perform m stmt)
  (let: ([op : PrimitiveCommand (PerformStatement-op stmt)])
        (cond
          [(SetToplevel!? op)
           (env-mutate m 
                       (SetToplevel!-depth op)
                       (toplevel-mutate (ensure-toplevel (env-ref m (SetToplevel!-depth op)))
                                        (SetToplevel!-pos op)
                                        
                                        (machine-val m)))]
          [(CheckToplevelBound!? op)
           (let: ([a-top : toplevel (ensure-toplevel (env-ref m (CheckToplevelBound!-depth op)))])
                 (cond
                   [(undefined? (list-ref (toplevel-vals a-top) (CheckToplevelBound!-pos op)))
                    (error 'check-toplevel-bound! "Unbound identifier ~s" (CheckToplevelBound!-name op))]
                   [else
                    m]))]

          [(ExtendEnvironment/Prefix!? op)
           (env-push m 
                     (make-toplevel (map lookup-primitive 
                                         (ExtendEnvironment/Prefix!-names op))))]
          [(InstallClosureValues!? op)
           (error 'step-perform)])))

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
     (list-ref (machine-env m) (EnvLexicalReference-depth an-oparg))]
    [(EnvWholePrefixReference? an-oparg)
     (unless (toplevel? (list-ref (machine-env m)
                                  (EnvWholePrefixReference-depth an-oparg)))
       (error 'evaluate-oparg "Internal error: not a toplevel at depth ~s"
              (EnvWholePrefixReference-depth an-oparg)))
     (list-ref (machine-env m) (EnvWholePrefixReference-depth an-oparg))]))


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


(: current-instruction (machine -> Statement))
(define (current-instruction m)
  (match m
    [(struct machine (val proc env control pc text))
     (vector-ref text pc)]))

  

(: val-update (machine SlotValue -> machine))
(define (val-update m v)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine v proc env control pc text)]))

(: proc-update (machine SlotValue -> machine))
(define (proc-update m v)
    (match m
    [(struct machine (val proc env control pc text))
     (make-machine val v env control pc text)]))

(: env-push (machine SlotValue -> machine))
(define (env-push m v)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc (cons v env) control pc text)]))

(: env-ref (machine Natural -> Any))
(define (env-ref m i)  
  (match m
    [(struct machine (val proc env control pc text))
     (list-ref env i)]))

(: env-mutate (machine Natural SlotValue -> machine))
(define (env-mutate m i v)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc (list-replace env i v) control pc text)]))

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
    [(struct machine (val proc env control pc text))
     (make-machine val proc (append (take env skip)
                                    (drop env (+ skip n)))
                   control pc text)]))


(: control-push (machine Symbol -> machine))
(define (control-push m l)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc env (cons (make-frame l) control) pc text)]))


(: control-pop (machine -> (values machine Symbol)))
(define (control-pop m)
  (match m
    [(struct machine (val proc env control pc text))
     (values (make-machine val proc env (rest control) pc text)
             (frame-return (first control)))]))

(: increment-pc (machine -> machine))
(define (increment-pc m)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc env control (add1 pc) text)]))


(: jump (machine Symbol -> machine))
;; Jumps directly to the instruction at the given label.
(define (jump m l)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc env control (vector-find text l) text)]))


(: vector-find (All (A) (Vectorof A) A -> Natural))
(define (vector-find vec x)
  (let: loop : Natural ([i : Natural 0])
        (cond
          [(eq? (vector-ref vec i) x)
           i]
          [else
           (loop (add1 i))])))


(: toplevel-mutate (toplevel Natural Any -> toplevel))
(define (toplevel-mutate a-top index v)
  (make-toplevel (append (take (toplevel-vals a-top) index)
                         (list v)
                         (drop (toplevel-vals a-top) (add1 index)))))
                 