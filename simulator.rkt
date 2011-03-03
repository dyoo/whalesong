#lang typed/racket/base

;; An evaluator for the intermediate language, so I can do experiments.

(require "il-structs.rkt"
         "simulator-structs.rkt"
         racket/list
         racket/match)

(provide new-machine can-step? step)


(: new-machine ((Listof Statement) -> machine))
(define (new-machine program-text)
  (make-machine (void) (void) '() '() 0 (list->vector program-text)))


(: can-step? (machine -> Boolean))
;; Produces true if we can make a further step in the simulation.
(define (can-step? m)
  (< (machine-pc m)
     (vector-length (machine-text m))))


(: step (machine -> machine))
;; Take one simulation step.
(define (step m)
  (let: ([i : Statement (current-instruction m)])
        (cond
          [(symbol? i)
           (increment-pc m)]
          [(AssignImmediateStatement? i)
           (error 'step)]
          [(AssignPrimOpStatement? i)
           (error 'step)]
          [(PerformStatement? i)
           (error 'step)]
          [(GotoStatement? i)
           (step-goto m i)]
          [(TestAndBranchStatement? i)
           (error 'step)]
          [(PopEnvironment? i)
           (error 'step)]
          [(PushEnvironment? i)
           (error 'step)]
          [(PushControlFrame? i)
           (error 'step)]
          [(PopControlFrame? i)
           (error 'step)])))

    
(: step-goto (machine GotoStatement -> machine))
(define (step-goto m a-goto)
  (let: ([t : (U Label Reg) (GotoStatement-target a-goto)])
        (cond [(Label? t)
               (jump m (Label-name t))]
              [(Reg? t)
               (let: ([reg : RegisterSymbol (Reg-name t)])
                     (cond [(AtomicRegisterSymbol? reg)
                            (cond [(eq? reg 'val)
                                   (jump m (ensure-symbol (machine-val m)))]
                                  [(eq? reg 'proc)
                                   (jump m (ensure-symbol (machine-proc m)))])]
                           [else
                            (error 'step-goto "Register '~s is supposed to be either 'val or 'proc"
                                   reg)]))])))


(: ensure-symbol (Any -> Symbol))
;; Make sure the value is a symbol.
(define (ensure-symbol v)
  (cond
    [(symbol? v)
     v]
    [else
     (error 'ensure-symbol)]))
  
  

(: current-instruction (machine -> Statement))
(define (current-instruction m)
  (match m
    [(struct machine (val proc env control pc text))
     (vector-ref text pc)]))

  

(: val-update (machine Any -> machine))
(define (val-update m v)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine v proc env control pc text)]))

(: proc-update (machine Any -> machine))
(define (proc-update m v)
    (match m
    [(struct machine (val proc env control pc text))
     (make-machine val v env control pc text)]))

(: env-push (machine Any -> machine))
(define (env-push m v)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc (cons v env) control pc text)]))

(: env-ref (machine Natural -> Any))
(define (env-ref m i)  
  (match m
    [(struct machine (val proc env control pc text))
     (list-ref env i)]))

(: env-mutate (machine Natural Any -> machine))
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


(: control-pop (machine Symbol -> (values machine Symbol)))
(define (control-pop m l)
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
;; Jumps directly to the instruction right after the given label.
(define (jump m l)
  (match m
    [(struct machine (val proc env control pc text))
     (make-machine val proc env control (add1 (vector-find text l)) text)]))


(: vector-find (All (A) (Vectorof A) A -> Natural))
(define (vector-find vec x)
  (let: loop : Natural ([i : Natural 0])
        (cond
          [(eq? (vector-ref vec i) x)
           i]
          [else
           (loop (add1 i))])))