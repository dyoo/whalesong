#lang typed/racket/base

(require "../compiler/il-structs.rkt"
         "../compiler/expression-structs.rkt"
         "../compiler/lexical-structs.rkt"
         racket/list)

(provide assemble-oparg
         assemble-target
         assemble-const
         assemble-lexical-reference
         assemble-prefix-reference
         assemble-whole-prefix-reference
         assemble-reg
         assemble-label
         assemble-listof-assembled-values
	 assemble-default-continuation-prompt-tag
	 assemble-env-reference/closure-capture
	 assemble-arity
	 assemble-jump
	 assemble-display-name
	 assemble-location
         assemble-numeric-constant)

(require/typed typed/racket/base
               [regexp-split (Regexp String -> (Listof String))])


(: assemble-oparg (OpArg -> String))
(define (assemble-oparg v)
  (cond 
    [(Reg? v)
     (assemble-reg v)]
    [(Label? v)
     (assemble-label v)]
    [(Const? v)
     (assemble-const v)]
    [(EnvLexicalReference? v)
     (assemble-lexical-reference v)]
    [(EnvPrefixReference? v)
     (assemble-prefix-reference v)]
    [(EnvWholePrefixReference? v)
     (assemble-whole-prefix-reference v)]
    [(SubtractArg? v)
     (assemble-subtractarg v)]
    [(ControlStackLabel? v)
     (assemble-control-stack-label v)]
    [(ControlStackLabel/MultipleValueReturn? v)
     (assemble-control-stack-label/multiple-value-return v)]
    [(ControlFrameTemporary? v)
     (assemble-control-frame-temporary v)]
    [(CompiledProcedureEntry? v)
     (assemble-compiled-procedure-entry v)]
    [(CompiledProcedureClosureReference? v)
     (assemble-compiled-procedure-closure-reference v)]
    [(PrimitiveKernelValue? v)
     (assemble-primitive-kernel-value v)]
    [(ModuleEntry? v)
     (assemble-module-entry v)]
    [(IsModuleInvoked? v)
     (assemble-is-module-invoked v)]
    [(IsModuleLinked? v)
     (assemble-is-module-linked v)]
    [(VariableReference? v)
     (assemble-variable-reference v)]))




(: assemble-target (Target -> String))
(define (assemble-target target)
  (cond
    [(eq? target 'proc)
     "MACHINE.proc"]
    [(eq? target 'val)
     "MACHINE.val"]
    [(eq? target 'argcount)
     "MACHINE.argcount"]
    [(EnvLexicalReference? target)
     (assemble-lexical-reference target)]
    [(EnvPrefixReference? target)
     (assemble-prefix-reference target)]
    [(PrimitivesReference? target)
     (format "RUNTIME.Primitives[~s]" (symbol->string (PrimitivesReference-name target)))]
    [(ControlFrameTemporary? target)
     (assemble-control-frame-temporary target)]
    [(ModulePrefixTarget? target)
     (format "MACHINE.modules[~s].prefix"
             (symbol->string (ModuleLocator-name (ModulePrefixTarget-path target))))]))


(: assemble-control-frame-temporary (ControlFrameTemporary -> String))
(define (assemble-control-frame-temporary t)
  (format "MACHINE.control[MACHINE.control.length-1].~a"
          (ControlFrameTemporary-name t)))

;; fixme: use js->string
(: assemble-const (Const -> String))
(define (assemble-const stmt)
  (let: loop : String ([val : Any (Const-const stmt)])
        (cond [(symbol? val)
               (format "~s" (symbol->string val))]
              [(pair? val)
               (format "RUNTIME.makePair(~a, ~a)" 
                       (loop (car val))
                       (loop (cdr val)))]
              [(boolean? val)
               (if val "true" "false")]
              [(void? val)
               "RUNTIME.VOID"]
              [(empty? val)
               (format "RUNTIME.NULL")]
              [(number? val)
               (assemble-numeric-constant val)]
              [else
               (format "~s" val)])))

(: assemble-listof-assembled-values ((Listof String) -> String))
(define (assemble-listof-assembled-values vals)
  (let loop ([vals vals])
    (cond
      [(empty? vals)
       "RUNTIME.NULL"]
      [else
       (format "RUNTIME.makePair(~a, ~a)" (first vals) (loop (rest vals)))])))



;; Slightly ridiculous definition, but I need it to get around what appear to
;; be Typed Racket bugs in its numeric tower.
(define-predicate int? Integer)



(: assemble-numeric-constant (Number -> String))
(define (assemble-numeric-constant a-num)

  (: floating-number->js (Real -> String))
  (define (floating-number->js a-num)
    (cond
     [(eqv? a-num -0.0)
      "jsnums.negative_zero"]
     [(eqv? a-num +inf.0)
      "jsnums.inf"]
     [(eqv? a-num -inf.0)
      "jsnums.negative_inf"]
     [(eqv? a-num +nan.0)
      "jsnums.nan"]
     [else
      (string-append "jsnums.makeFloat(" (number->string a-num) ")")]))

  ;; FIXME: fix the type signature when typed-racket isn't breaking on
  ;; (define-predicate ExactRational? (U Exact-Rational))
  (: rational-number->js (Real -> String))
  (define (rational-number->js a-num)
    (cond [(= (denominator a-num) 1)
           (string-append (integer->js (ensure-integer (numerator a-num))))]
          [else
           (string-append "jsnums.makeRational("
                          (integer->js (ensure-integer (numerator a-num)))
                          ", "
                          (integer->js (ensure-integer (denominator a-num)))
                          ")")]))

  
  (: ensure-integer (Any -> Integer))
  (define (ensure-integer x)
    (if (int? x)
        x
        (error "not an integer: ~e" x)))
  


  (: integer->js (Integer -> String))
  (define (integer->js an-int)
    (cond
     ;; non-overflow case
     [(< (abs an-int) 9e15)
      (number->string an-int)]
     ;; overflow case
     [else
      (string-append "jsnums.makeBignum("
                     (format "~s" (number->string an-int))
                     ")")]))

  (cond 
   [(and (exact? a-num) (rational? a-num))
    (rational-number->js a-num)]
   
   [(real? a-num)
    (floating-number->js a-num)]
   
   [(complex? a-num)
    (string-append "jsnums.makeComplex("
                   (assemble-numeric-constant (real-part a-num))
                   ", "
                   (assemble-numeric-constant (imag-part a-num))
                   ")")]))


















   



(: assemble-lexical-reference (EnvLexicalReference -> String))
(define (assemble-lexical-reference a-lex-ref)
  (if (EnvLexicalReference-unbox? a-lex-ref)
      (format "MACHINE.env[MACHINE.env.length - 1 - ~a][0]"
              (EnvLexicalReference-depth a-lex-ref))
      (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
              (EnvLexicalReference-depth a-lex-ref))))

(: assemble-prefix-reference (EnvPrefixReference -> String))
(define (assemble-prefix-reference a-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a][~a]"
          (EnvPrefixReference-depth a-ref)
          (EnvPrefixReference-pos a-ref)))

(: assemble-whole-prefix-reference (EnvWholePrefixReference -> String))
(define (assemble-whole-prefix-reference a-prefix-ref)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          (EnvWholePrefixReference-depth a-prefix-ref)))


(: assemble-reg (Reg -> String))
(define (assemble-reg a-reg)
  (string-append "MACHINE." (symbol->string (Reg-name a-reg))))



(: assemble-label (Label -> String))
(define (assemble-label a-label)
  (let ([chunks
         (regexp-split #rx"[^a-zA-Z0-9]+"
                       (symbol->string (Label-name a-label)))])
    (cond
      [(empty? chunks)
       (error "impossible: empty label ~s" a-label)]
      [(empty? (rest chunks))
       (string-append "_" (first chunks))]
      [else
       (string-append "_"
                      (first chunks)
                      (apply string-append (map string-titlecase (rest chunks))))])))



(: assemble-subtractarg (SubtractArg -> String))
(define (assemble-subtractarg s)
  (format "(~a - ~a)"
          (assemble-oparg (SubtractArg-lhs s))
          (assemble-oparg (SubtractArg-rhs s))))


(: assemble-control-stack-label (ControlStackLabel -> String))
(define (assemble-control-stack-label a-csl)
  "MACHINE.control[MACHINE.control.length-1].label")


(: assemble-control-stack-label/multiple-value-return (ControlStackLabel/MultipleValueReturn -> String))
(define (assemble-control-stack-label/multiple-value-return a-csl)
  "MACHINE.control[MACHINE.control.length-1].label.multipleValueReturn")



(: assemble-compiled-procedure-entry (CompiledProcedureEntry -> String))
(define (assemble-compiled-procedure-entry a-compiled-procedure-entry)
  (format "(~a).label"
          (assemble-oparg (CompiledProcedureEntry-proc a-compiled-procedure-entry))))


(: assemble-compiled-procedure-closure-reference (CompiledProcedureClosureReference -> String))
(define (assemble-compiled-procedure-closure-reference a-ref)
  (format "(~a).closedVals[(~a).closedVals.length - 1 - ~a]"
          (assemble-oparg (CompiledProcedureClosureReference-proc a-ref))
          (assemble-oparg (CompiledProcedureClosureReference-proc a-ref))
          (CompiledProcedureClosureReference-n a-ref)))



(: assemble-default-continuation-prompt-tag (-> String))
(define (assemble-default-continuation-prompt-tag)
  "RUNTIME.DEFAULT_CONTINUATION_PROMPT_TAG")



(: assemble-env-reference/closure-capture (Natural -> String))
;; When we're capturing the values for a closure, we need to not unbox
;; lexical references: they must remain boxes.  So all we need is 
;; the depth into the environment.
(define (assemble-env-reference/closure-capture depth)
  (format "MACHINE.env[MACHINE.env.length - 1 - ~a]"
          depth))



(define-predicate natural? Natural)

(: assemble-arity (Arity -> String))
(define (assemble-arity an-arity)
  (cond
   [(natural? an-arity)
    (number->string an-arity)]
   [(ArityAtLeast? an-arity)
    (format "(new RUNTIME.ArityAtLeast(~a))"
            (ArityAtLeast-value an-arity))]
   [(listof-atomic-arity? an-arity)
    (assemble-listof-assembled-values
     (map
      (lambda: ([atomic-arity : (U Natural ArityAtLeast)])
	       (cond
		[(natural? atomic-arity)
		 (number->string atomic-arity)]
		[(ArityAtLeast? atomic-arity)
		 (format "(new RUNTIME.ArityAtLeast(~a))"
                         (ArityAtLeast-value atomic-arity))]))
      an-arity))]))





(: assemble-jump (OpArg -> String))
(define (assemble-jump target)
  (format "return (~a)(MACHINE);" (assemble-oparg target)))





(: assemble-display-name ((U Symbol LamPositionalName) -> String))
(define (assemble-display-name name)
  (cond
   [(symbol? name)
    (format "~s" (symbol->string name))]
   [(LamPositionalName? name)
    ;; FIXME: record more interesting information here.
    (format "~s" (symbol->string (LamPositionalName-name name)))]))




(: assemble-location ((U Reg Label) -> String))
(define (assemble-location a-location)
  (cond
     [(Reg? a-location)
      (assemble-reg a-location)]
     [(Label? a-location)
      (assemble-label a-location)]))


(: assemble-primitive-kernel-value (PrimitiveKernelValue -> String))
(define (assemble-primitive-kernel-value a-prim)
  (format "MACHINE.primitives[~s]" (symbol->string (PrimitiveKernelValue-id a-prim))))



(: assemble-module-entry (ModuleEntry -> String))
(define (assemble-module-entry entry)
  (format "MACHINE.modules[~s].label"
          (symbol->string (ModuleLocator-name (ModuleEntry-name entry)))))


(: assemble-is-module-invoked (IsModuleInvoked -> String))
(define (assemble-is-module-invoked entry)
  (format "MACHINE.modules[~s].isInvoked"
          (symbol->string (ModuleLocator-name (IsModuleInvoked-name entry)))))


(: assemble-is-module-linked (IsModuleLinked -> String))
(define (assemble-is-module-linked entry)
  (format "(MACHINE.modules[~s] !== undefined)"
          (symbol->string (ModuleLocator-name (IsModuleLinked-name entry)))))



(: assemble-variable-reference (VariableReference -> String))
(define (assemble-variable-reference varref)
  (let ([t (VariableReference-toplevel varref)])
    (format "(new RUNTIME.VariableReference(MACHINE.env[MACHINE.env.length - 1 - ~a], ~a))"
            (ToplevelRef-depth t)
            (ToplevelRef-pos t))))