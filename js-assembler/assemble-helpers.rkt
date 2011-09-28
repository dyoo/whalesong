#lang typed/racket/base/no-check

(require "../compiler/il-structs.rkt"
         "../compiler/expression-structs.rkt"
         "../compiler/lexical-structs.rkt"
         "../compiler/arity-structs.rkt"
         "assemble-structs.rkt"
         racket/list
         racket/string
         racket/match)




(provide assemble-oparg
         assemble-target
         assemble-const
         assemble-lexical-reference
         assemble-prefix-reference
         assemble-whole-prefix-reference
         assemble-reg
         munge-label-name
         assemble-label
         assemble-listof-assembled-values
	 assemble-default-continuation-prompt-tag
	 assemble-env-reference/closure-capture
	 assemble-arity
	 assemble-jump
	 assemble-display-name
	 assemble-location
         assemble-numeric-constant

         block-looks-like-context-expected-values?)

(require/typed typed/racket/base
               [regexp-split (Regexp String -> (Listof String))])


(: assemble-oparg (OpArg Blockht -> String))
(define (assemble-oparg v blockht)
  (cond 
    [(Reg? v)
     (assemble-reg v)]
    [(Label? v)
     (assemble-label v blockht)]
    [(Const? v)
     (assemble-const v)]
    [(EnvLexicalReference? v)
     (assemble-lexical-reference v)]
    [(EnvPrefixReference? v)
     (assemble-prefix-reference v)]
    [(EnvWholePrefixReference? v)
     (assemble-whole-prefix-reference v)]
    [(SubtractArg? v)
     (assemble-subtractarg v blockht)]
    [(ControlStackLabel? v)
     (assemble-control-stack-label v)]
    [(ControlStackLabel/MultipleValueReturn? v)
     (assemble-control-stack-label/multiple-value-return v)]
    [(ControlFrameTemporary? v)
     (assemble-control-frame-temporary v)]
    [(CompiledProcedureEntry? v)
     (assemble-compiled-procedure-entry v blockht)]
    [(CompiledProcedureClosureReference? v)
     (assemble-compiled-procedure-closure-reference v blockht)]
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






(: assemble-target (Target -> (String -> String)))
(define (assemble-target target)
  (cond
   [(PrimitivesReference? target)
    (lambda: ([rhs : String])
             (format "RT.Primitives[~s]=RT.Primitives[~s]||~a;"
                     (symbol->string (PrimitivesReference-name target))
                     (symbol->string (PrimitivesReference-name target))
                     rhs))]
   [else
    (lambda: ([rhs : String])
             (format "~a=~a;"
                     (cond
                      [(eq? target 'proc)
                       "M.p"]
                      [(eq? target 'val)
                       "M.v"]
                      [(eq? target 'argcount)
                       "M.a"]
                      [(EnvLexicalReference? target)
                       (assemble-lexical-reference target)]
                      [(EnvPrefixReference? target)
                       (assemble-prefix-reference target)]
                      [(ControlFrameTemporary? target)
                       (assemble-control-frame-temporary target)]
                      [(ModulePrefixTarget? target)
                       (format "M.modules[~s].prefix"
                               (symbol->string (ModuleLocator-name (ModulePrefixTarget-path target))))])
                     rhs))]))



(: assemble-control-frame-temporary (ControlFrameTemporary -> String))
(define (assemble-control-frame-temporary t)
  (format "M.c[M.c.length-1].~a"
          (ControlFrameTemporary-name t)))

;; fixme: use js->string
(: assemble-const (Const -> String))
(define (assemble-const stmt)
  (let: loop : String ([val : const-value (Const-const stmt)])
        (cond [(symbol? val)
               (format "RT.makeSymbol(~s)" (symbol->string val))]
              [(pair? val)
               (format "RT.makePair(~a,~a)" 
                       (loop (car val))
                       (loop (cdr val)))]
              [(boolean? val)
               (if val "true" "false")]
              [(void? val)
               "RT.VOID"]
              [(empty? val)
               (format "RT.NULL")]
              [(number? val)
               (assemble-numeric-constant val)]
              [(string? val)
               (format "~s" val)]
              [(char? val)
               (format "RT.makeChar(~s)" (string val))]
              [(bytes? val)
               ;; This needs to be an array, because this may contain
               ;; a LOT of elements, and certain JS evaluators will break
               ;; otherewise.
               (format "RT.makeBytes([~a])"
                       (string-join (for/list ([a-byte val])
                                      (number->string a-byte))
                                    ","))]
              [(path? val)
               (format "RT.makePath(~s)"
                       (path->string val))]
              [(vector? val)
               (format "RT.makeVector(~a,[~a])"
                       (vector-length val)
                       (string-join (for/list ([elt (vector->list val)])
                                       (loop elt))
                                    ","))]
              [(box? val)
               (format "RT.makeBox(~s)"
                       (loop (unbox val)))])))



(: assemble-listof-assembled-values ((Listof String) -> String))
(define (assemble-listof-assembled-values vals)
  (let loop ([vals vals])
    (cond
      [(empty? vals)
       "RT.NULL"]
      [else
       (format "RT.makePair(~a,~a)" (first vals) (loop (rest vals)))])))



;; Slightly ridiculous definition, but I need it to get around what appear to
;; be Typed Racket bugs in its numeric tower.
(define-predicate int? Integer)



(: assemble-numeric-constant (Number -> String))
(define (assemble-numeric-constant a-num)

  (: floating-number->js (Real -> String))
  (define (floating-number->js a-num)
    (cond
     [(eqv? a-num -0.0)
      "RT.NEGATIVE_ZERO"]
     [(eqv? a-num +inf.0)
      "RT.INF"]
     [(eqv? a-num -inf.0)
      "RT.NEGATIVE_INF"]
     [(eqv? a-num +nan.0)
      "RT.NAN"]
     [else
      (string-append "RT.makeFloat(" (number->string a-num) ")")]))

  ;; FIXME: fix the type signature when typed-racket isn't breaking on
  ;; (define-predicate ExactRational? (U Exact-Rational))
  (: rational-number->js (Real -> String))
  (define (rational-number->js a-num)
    (cond [(= (denominator a-num) 1)
           (string-append (integer->js (ensure-integer (numerator a-num))))]
          [else
           (string-append "RT.makeRational("
                          (integer->js (ensure-integer (numerator a-num)))
                          ","
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
      (string-append "RT.makeBignum("
                     (format "~s" (number->string an-int))
                     ")")]))

  (cond 
   [(and (exact? a-num) (rational? a-num))
    (rational-number->js a-num)]
   
   [(real? a-num)
    (floating-number->js a-num)]
   
   [(complex? a-num)
    (string-append "RT.makeComplex("
                   (assemble-numeric-constant (real-part a-num))
                   ","
                   (assemble-numeric-constant (imag-part a-num))
                   ")")]))


















   



(: assemble-lexical-reference (EnvLexicalReference -> String))
(define (assemble-lexical-reference a-lex-ref)
  (if (EnvLexicalReference-unbox? a-lex-ref)
      (format "M.e[M.e.length-~a][0]"
              (add1 (EnvLexicalReference-depth a-lex-ref)))
      (format "M.e[M.e.length-~a]"
              (add1 (EnvLexicalReference-depth a-lex-ref)))))

(: assemble-prefix-reference (EnvPrefixReference -> String))
(define (assemble-prefix-reference a-ref)
  (format "M.e[M.e.length-~a][~a]"
          (add1 (EnvPrefixReference-depth a-ref))
          (EnvPrefixReference-pos a-ref)))

(: assemble-whole-prefix-reference (EnvWholePrefixReference -> String))
(define (assemble-whole-prefix-reference a-prefix-ref)
  (format "M.e[M.e.length-~a]"
          (add1 (EnvWholePrefixReference-depth a-prefix-ref))))


(: assemble-reg (Reg -> String))
(define (assemble-reg a-reg)
  (let ([name (Reg-name a-reg)])
    (cond
     [(eq? name 'proc)
      "M.p"]
     [(eq? name 'val)
      "M.v"]
     [(eq? name 'argcount)
      "M.a"])))


(: munge-label-name (Label -> String))
(define (munge-label-name a-label)
  (define chunks
    (regexp-split #rx"[^a-zA-Z0-9]+"
                  (symbol->string (Label-name a-label))))
  (cond
   [(empty? chunks)
    (error "impossible: empty label ~s" a-label)]
   [(empty? (rest chunks))
    (string-append "_" (first chunks))]
   [else
    (string-append "_"
                   (first chunks)
                   (apply string-append (map string-titlecase (rest chunks))))]))



(: assemble-label (Label Blockht -> String))
(define (assemble-label a-label Blockht)
  (munge-label-name a-label))



(: assemble-subtractarg (SubtractArg Blockht -> String))
(define (assemble-subtractarg s blockht)
  (format "(~a-~a)"
          (assemble-oparg (SubtractArg-lhs s) blockht)
          (assemble-oparg (SubtractArg-rhs s) blockht)))


(: assemble-control-stack-label (ControlStackLabel -> String))
(define (assemble-control-stack-label a-csl)
  "M.c[M.c.length-1].label")


(: assemble-control-stack-label/multiple-value-return (ControlStackLabel/MultipleValueReturn -> String))
(define (assemble-control-stack-label/multiple-value-return a-csl)
  "(M.c[M.c.length-1].label.mvr||RT.si_context_expected_1)")



(: assemble-compiled-procedure-entry (CompiledProcedureEntry Blockht -> String))
(define (assemble-compiled-procedure-entry a-compiled-procedure-entry blockht)
  (format "(~a).label"
          (assemble-oparg (CompiledProcedureEntry-proc a-compiled-procedure-entry)
                          blockht)))


(: assemble-compiled-procedure-closure-reference (CompiledProcedureClosureReference Blockht -> String))
(define (assemble-compiled-procedure-closure-reference a-ref blockht)
  (format "(~a).closedVals[(~a).closedVals.length - ~a]"
          (assemble-oparg (CompiledProcedureClosureReference-proc a-ref) blockht)
          (assemble-oparg (CompiledProcedureClosureReference-proc a-ref) blockht)
          (add1 (CompiledProcedureClosureReference-n a-ref))))



(: assemble-default-continuation-prompt-tag (-> String))
(define (assemble-default-continuation-prompt-tag)
  "RT.DEFAULT_CONTINUATION_PROMPT_TAG")



(: assemble-env-reference/closure-capture (Natural -> String))
;; When we're capturing the values for a closure, we need to not unbox
;; lexical references: they must remain boxes.  So all we need is 
;; the depth into the environment.
(define (assemble-env-reference/closure-capture depth)
  (format "M.e[M.e.length-~a]"
          (add1 depth)))



(define-predicate natural? Natural)

(: assemble-arity (Arity -> String))
(define (assemble-arity an-arity)
  (cond
   [(natural? an-arity)
    (number->string an-arity)]
   [(ArityAtLeast? an-arity)
    (format "(RT.makeArityAtLeast(~a))"
            (ArityAtLeast-value an-arity))]
   [(listof-atomic-arity? an-arity)
    (assemble-listof-assembled-values
     (map
      (lambda: ([atomic-arity : (U Natural ArityAtLeast)])
	       (cond
		[(natural? atomic-arity)
		 (number->string atomic-arity)]
		[(ArityAtLeast? atomic-arity)
		 (format "(RT.makeArityAtLeast(~a))"
                         (ArityAtLeast-value atomic-arity))]))
      an-arity))]))






(: assemble-jump (OpArg Blockht -> String))
(define (assemble-jump target blockht)

  (define (default)
    (format "return(~a)(M);" (assemble-oparg target blockht)))
  
  ;; Optimization: if the target of the jump goes to a block whose
  ;; only body is a si-context-expected_1, then jump directly to that code.
  (cond
   [(Label? target)
    (define target-block (hash-ref blockht (Label-name target)))
    (cond
     [(block-looks-like-context-expected-values? target-block)
      =>
      (lambda (expected)
        (format "RT.si_context_expected(~a)(M);\n" expected))]
     [else
      (default)])]
   [else
    (default)]))



(: block-looks-like-context-expected-values? (BasicBlock -> (U Natural False)))
(define (block-looks-like-context-expected-values? a-block)
  (match (BasicBlock-stmts a-block)
    [(list (struct PerformStatement ((struct RaiseContextExpectedValuesError! (expected))))
           stmts ...)
     expected]
    [else
     #f]))





(: assemble-display-name ((U Symbol LamPositionalName) -> String))
(define (assemble-display-name name)
  (cond
   [(symbol? name)
    (format "~s" (symbol->string name))]
   [(LamPositionalName? name)
    ;; FIXME: record more interesting information here.
    (format "~s" (symbol->string (LamPositionalName-name name)))]))




(: assemble-location ((U Reg Label) Blockht -> String))
(define (assemble-location a-location blockht)
  (cond
     [(Reg? a-location)
      (assemble-reg a-location)]
     [(Label? a-location)
      (assemble-label a-location blockht)]))


(: assemble-primitive-kernel-value (PrimitiveKernelValue -> String))
(define (assemble-primitive-kernel-value a-prim)
  (format "M.primitives[~s]" (symbol->string (PrimitiveKernelValue-id a-prim))))



(: assemble-module-entry (ModuleEntry -> String))
(define (assemble-module-entry entry)
  (format "M.modules[~s].label"
          (symbol->string (ModuleLocator-name (ModuleEntry-name entry)))))


(: assemble-is-module-invoked (IsModuleInvoked -> String))
(define (assemble-is-module-invoked entry)
  (format "M.modules[~s].isInvoked"
          (symbol->string (ModuleLocator-name (IsModuleInvoked-name entry)))))


(: assemble-is-module-linked (IsModuleLinked -> String))
(define (assemble-is-module-linked entry)
  (format "(M.modules[~s]!==undefined)"
          (symbol->string (ModuleLocator-name (IsModuleLinked-name entry)))))



(: assemble-variable-reference (VariableReference -> String))
(define (assemble-variable-reference varref)
  (let ([t (VariableReference-toplevel varref)])
    (format "(new RT.VariableReference(M.e[M.e.length-~a],~a))"
            (add1 (ToplevelRef-depth t))
            (ToplevelRef-pos t))))