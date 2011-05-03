#lang typed/racket/base

(require "il-structs.rkt"
         "lexical-structs.rkt"
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
	 assemble-location)

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
    [(CompiledProcedureEntry? v)
     (assemble-compiled-procedure-entry v)]
    [(ControlFrameTemporary? v)
     (assemble-control-frame-temporary v)]))




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
     (format "MACHINE.primitives[~s]" (symbol->string (PrimitivesReference-name target)))]
    [(ControlFrameTemporary? target)
     (assemble-control-frame-temporary target)]))


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
               (format "[~a, ~a]" 
                       (loop (car val))
                       (loop (cdr val)))]
              [(boolean? val)
               (if val "true" "false")]
              [(void? val)
               "null"]
              [(empty? val)
               (format "RUNTIME.NULL")]
              [(number? val)
               (format "(~s)" val)]
              [else
               (format "~s" val)])))

(: assemble-listof-assembled-values ((Listof String) -> String))
(define (assemble-listof-assembled-values vals)
  (let loop ([vals vals])
    (cond
      [(empty? vals)
       "RUNTIME.NULL"]
      [else
       (format "[~a, ~a]" (first vals) (loop (rest vals)))])))




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
    (format "~a" an-arity)]
   [(ArityAtLeast? an-arity)
    (format "(new RUNTIME.ArityAtLeast(~a))" (ArityAtLeast-value an-arity))]
   [(listof-atomic-arity? an-arity)
    (assemble-listof-assembled-values
     (map
      (lambda: ([atomic-arity : (U Natural ArityAtLeast)])
	       (cond
		[(natural? atomic-arity)
		 (format "~a" an-arity)]
		[(ArityAtLeast? an-arity)
		 (format "(new RUNTIME.ArityAtLeast(~a))" (ArityAtLeast-value an-arity))]
		;; Can't seem to make the type checker happy without this...
		[else (error 'assemble-arity)]))
      an-arity))]))





(: assemble-jump (OpArg -> String))
(define (assemble-jump target)
  (format "return (~a)(MACHINE);" (assemble-oparg target)))





(: assemble-display-name ((U Symbol False) -> String))
(define (assemble-display-name symbol-or-string)
  (if (symbol? symbol-or-string)
       (format "~s" (symbol->string symbol-or-string))
       "false"))






(: assemble-location ((U Reg Label) -> String))
(define (assemble-location a-location)
  (cond
     [(Reg? a-location)
      (assemble-reg a-location)]
     [(Label? a-location)
      (assemble-label a-location)]))


