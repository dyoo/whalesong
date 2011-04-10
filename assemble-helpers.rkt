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
         assemble-listof-assembled-values)


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
     (assemble-subtractarg v)]))




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
     (format "MACHINE.primitives[~s]" (symbol->string (PrimitivesReference-name target)))]))



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
  (symbol->string (Label-name a-label)))

(: assemble-subtractarg (SubtractArg -> String))
(define (assemble-subtractarg s)
  (format "(~a - ~a)"
          (assemble-oparg (SubtractArg-lhs s))
          (assemble-oparg (SubtractArg-rhs s))))
