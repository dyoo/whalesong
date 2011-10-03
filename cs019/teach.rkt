#lang racket/base

(require (for-template "../lang/base.rkt")
         (for-template "teach-runtime.rkt")
         "teachhelp.rkt"
         stepper/private/shared
         racket/list
         syntax/context
         syntax/kerncase
         syntax/stx)


(provide advanced-define/proc
         advanced-lambda/proc
         advanced-when/proc
         advanced-unless/proc
         advanced-set!/proc advanced-set!-continue/proc
         advanced-case/proc
         intermediate-local/proc)



  ;; verify-boolean is inserted to check for boolean results:
  (define (verify-boolean b where)
    (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
        (format "~a: question result is not true or false: ~e" where b)
        (current-continuation-marks)))))


;; A consistent pattern for stepper-skipto:
(define (stepper-ignore-checker stx)
  (stepper-syntax-property stx 'stepper-skipto '(syntax-e cdr syntax-e cdr car)))



(define (make-name-inventer)
  ;; Normally we'd use (make-syntax-introducer) because gensyming makes
  ;;  identifiers that play badly with exporting. But we don't have
  ;;  to worry about exporting in the teaching languages, while we do
  ;;  have to worry about mangled names.
  (lambda (id)
    (datum->syntax id 
                   (string->uninterned-symbol (symbol->string (syntax-e id)))
                   id)))

;; Check context for a `define' before even trying to
;; expand
(define-struct expanding-for-intermediate-local ())



;; Raise a syntax error:
(define (teach-syntax-error form stx detail msg . args)
  (let ([form (or form (first (flatten (syntax->datum stx))))]
        [msg (apply format msg args)])
    (if detail
        (raise-syntax-error form msg stx detail)
        (raise-syntax-error form msg stx))))


(define (teach-syntax-error* form stx details msg . args)
  (let ([exn (with-handlers ([exn:fail:syntax?
                              (lambda (x) x)])
               (apply teach-syntax-error form stx #f msg args))])
    (raise
     (make-exn:fail:syntax
      (exn-message exn)
      (exn-continuation-marks exn)
      details))))



(define (ensure-expression stx k)
  (if (memq (syntax-local-context) '(expression))
      (k)
      (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))



(define (something-else/kw stx)
  (if (identifier? stx)
      "a keyword"
      (something-else stx)))


;; Use for messages "expected ..., found <something else>"
(define (something-else v)
  (let ([v (syntax-e v)])
    (cond
     [(number? v) "a number"]
     [(string? v) "a string"]
     [(list? v) "a part"]
     [(struct? v) "an image"]
     [else "something else"])))



;; At the top level, wrap `defn' to first check for
;;  existing definitions of the `names'. The `names'
;;  argument is a syntax list of identifiers.
;; In a module context, just check the binding
;;  at compile time.
;; In either context, if `assign?' is true, then
;;  generate an unevaluated assignment that makes
;;  the identifier mutable.
(define (check-definitions-new who stx names defn assign)
  (cond
   [(eq? (syntax-local-context) 'top-level)
    (with-syntax ([defn defn]
                  [who who])
      (with-syntax ([(check ...)
                     (map (lambda (name)
                            (with-syntax ([name name])
                              ;; Make sure each check has the
                              ;; source location of the original
                              ;; expression:
                              (syntax/loc stx
                                (void) #;(check-top-level-not-defined 'who #'name))))
                          names)])
        (stepper-syntax-property 
         (syntax/loc stx
           (begin
             check ...
             defn)) 
         'stepper-skipto 
         (cons 'syntax-e
               (let loop ([l names])
                 (if (null? l)
                     `(syntax-e cdr car)
                     (cons 'cdr (loop (cdr l)))))))))]
   [(memq (syntax-local-context) '(module module-begin))
    (for-each (lambda (name)
                (let ([b (identifier-binding name)])
                  (when b
                    (teach-syntax-error
                     (syntax-e name)
                     name
                     #f
                     "this name was defined previously and cannot be re-defined"))))
              names)
    (if assign
        (with-syntax ([(name ...) (if (eq? assign #t)
                                      names
                                      assign)]
                      [made-up (gensym)]
                      [defn defn])
          (with-syntax ([made-up-defn (stepper-syntax-property 
                                       (with-syntax ([set! (datum->syntax stx 'set!)])
                                         (syntax (define made-up (lambda () (set! name 10) ...))))
                                       'stepper-skip-completely
                                       #t)])
            (syntax/loc stx
              (begin
                made-up-defn ;; (define made-up (lambda () (advanced-set! name 10) ...))
                defn))))
        defn)]
   [else defn]))


;; Same as above, but for one name
(define (check-definition-new who stx name defn assign)
  (check-definitions-new who stx (list name) defn assign))



(define (check-single-result-expr exprs where enclosing-expr will-bind)
  (check-single-expression where
                           "for the function body"
                           enclosing-expr
                           exprs
                           will-bind))



;; Use to generate nicer error messages than direct pattern
;; matching. The `where' argument is an English description
;; of the portion of the larger expression where a single
;; sub-expression was expected.
(define (check-single-expression who where stx exprs will-bind)
  (when (null? exprs)
    (teach-syntax-error
     who
     stx
     #f
     "expected an expression ~a, but nothing's there"
     where))
  (unless (null? (cdr exprs))
    ;; In case it's erroneous, to ensure left-to-right reading, let's
    ;;  try expanding the first expression. We have to use
    ;;  `will-bind' to avoid errors for unbound ids that will actually
    ;;  be bound. Since they're used as stopping points, we may miss
    ;;  some errors after all. It's worth a try, though. We also
    ;;  have to stop at advanced-set!, in case it's used with
    ;;  one of the identifiers in will-bind.
    (when will-bind
      (local-expand-for-error (car exprs) 'expression (cons (datum->syntax stx 'set!)
                                                            will-bind)))
    ;; First expression seems ok, report an error for 2nd and later:
    (teach-syntax-error
     who
     stx
     (cadr exprs)
     "expected only one expression ~a, but found ~a extra part~a"
     where
     (sub1 (length exprs))
     (if (> (length exprs) 2) "s" ""))))

(define (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))

;; The syntax error when a form's name doesn't follow a "("
(define (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "expected an open parenthesis before ~a, but found none" name))






(define (check-defined-lambda rhs)
  (syntax-case rhs ()
    [(lam . _)
     (and (identifier? #'lam)
          (or (free-identifier=? #'lam #'beginner-lambda)
              (free-identifier=? #'lam #'intermediate-pre-lambda)))
     (syntax-case rhs ()
       [(lam arg-seq lexpr ...)
        (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
        (let ([args (syntax->list (syntax arg-seq))])
          (for-each (lambda (arg)
                      (unless (identifier? arg)
                        (teach-syntax-error
                         'lambda
                         rhs
                         arg
                         "expected a variable, but found ~a"
                         (something-else/kw arg))))
                    args)
          (when (null? args)
            (teach-syntax-error
             'lambda
             rhs
             (syntax arg-seq)
             "expected at least one variable after lambda, but found none"))
          (let ([dup (check-duplicate-identifier args)])
            (when dup
              (teach-syntax-error
               'lambda
               rhs
               dup
               "found a variable that is used more than once: ~a"
               (syntax-e dup))))
          (check-single-result-expr (syntax->list (syntax (lexpr ...)))
                                    #f
                                    rhs
                                    args)
          'ok)]
       ;; Bad lambda because bad args:
       [(lam args . _)
        (teach-syntax-error
         'lambda
         rhs
         (syntax args)
         "expected at least one variable (in parentheses) after lambda, but found ~a"
         (something-else (syntax args)))]
       ;; Bad lambda, no args:
       [(lam)
        (teach-syntax-error
         'lambda
         rhs
         #f
         "expected at least one variable (in parentheses) after lambda, but nothing's there")]
       [_else 'ok])]
    [_else 'ok]))











    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; local
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (intermediate-local/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ (definition ...) . exprs)
	    (let ([defns (syntax->list (syntax (definition ...)))]
		  ;; The following context value lets teaching-language definition
		  ;;  forms know that it's ok to expand in this internal
		  ;;  definition context.
		  [int-def-ctx (build-expand-context (make-expanding-for-intermediate-local))])
	      (let* ([partly-expand (lambda (d)
                                      (local-expand
                                       d
                                       int-def-ctx
                                       (kernel-form-identifier-list)))]
                     [partly-expanded-defns
		      (map partly-expand defns)]
		     [flattened-defns
		      (let loop ([l partly-expanded-defns][origs defns])
			(apply
			 append
			 (map (lambda (d orig)
				(syntax-case d (begin define-values define-syntaxes)
				  ;; we don't have to check for ill-formed `define-values'
				  ;; or `define-syntaxes', because only macros can generate
				  ;; them
				  [(begin defn ...)
				   (let ([l (map partly-expand (syntax->list (syntax (defn ...))))])
				     (loop l l))]
				  [(define-values . _)
				   (list d)]
				  [(define-syntaxes . _)
				   (list d)]
				  [_else
				   (teach-syntax-error
				    'local
				    stx
				    orig
				    "expected a definition, but found ~a"
				    (something-else orig))]))
			      l origs)))]
		     [val-defns
		      (apply
		       append
		       (map (lambda (partly-expanded)
			      (syntax-case partly-expanded (define-values)
				[(define-values (id ...) expr)
				 (list partly-expanded)]
				[_else
				 null]))
			    flattened-defns))]
		     [stx-defns
		      (apply
		       append
		       (map (lambda (partly-expanded)
			      (syntax-case partly-expanded (define-syntaxes)
				[(define-syntaxes (id ...) expr)
				 (list partly-expanded)]
				[_else
				 null]))
			    flattened-defns))]
		     [get-ids (lambda (l)
				(apply
				 append
				 (map (lambda (partly-expanded)
					(syntax-case partly-expanded ()
					  [(_ (id ...) expr)
					   (syntax->list (syntax (id ...)))]))
				      l)))]
		     [val-ids (get-ids val-defns)]
		     [stx-ids (get-ids stx-defns)])
		(let ([dup (check-duplicate-identifier (append val-ids stx-ids))])
		  (when dup
		    (teach-syntax-error
		     'local
		     stx
		     dup
		     "~a was defined locally more than once"
		     (syntax-e dup)))
		  (let ([exprs (syntax->list (syntax exprs))])
		    (check-single-expression 'local
					     "after the local definitions"
					     stx
					     exprs
					     (append val-ids stx-ids)))
		  (with-syntax ([((d-v (def-id ...) def-expr) ...) val-defns]
				[(stx-def ...) stx-defns])
		    (with-syntax ([(((tmp-id def-id/prop) ...) ...)
				   ;; Generate tmp-ids that at least look like the defined
				   ;;  ids, for the purposes of error reporting, etc.:
				   (map (lambda (def-ids)
					  (map (lambda (def-id)
						 (list
						  (stepper-syntax-property
						   (datum->syntax
						    #f
						    (string->uninterned-symbol
						     (symbol->string (syntax-e def-id))))
						   'stepper-orig-name
						   def-id)
						  (syntax-property
						   def-id
						   'bind-as-variable
						   #t)))
					       (syntax->list def-ids)))
					(syntax->list (syntax ((def-id ...) ...))))])
		      (with-syntax ([(mapping ...)
				     (let ([mappers
					    (syntax->list
					     (syntax
					      ((define-syntaxes (def-id/prop ...)
						 (values
						  (make-undefined-check
						   (quote-syntax check-not-undefined)
						   (quote-syntax tmp-id))
						  ...))
					       ...)))])
				       (map syntax-track-origin
					    mappers
					    val-defns
					    (syntax->list (syntax (d-v ...)))))])
			(stepper-syntax-property
			 (quasisyntax/loc stx
			   (let ()
                             (#%stratified-body
                              (define #,(gensym) 1) ; this ensures that the expansion of 'local' looks
					            ; roughly the same, even if the local has no defs.
                              mapping ...
                              stx-def ...
                              (define-values (tmp-id ...) def-expr)
                              ...
                              . exprs)))
			 'stepper-hint
			 'comes-from-local)))))))]
	   [(_ def-non-seq . __)
	    (teach-syntax-error
	     'local
	     stx
	     (syntax def-non-seq)
	     "expected at least one definition (in square brackets) after local, but found ~a"
	     (something-else (syntax def-non-seq)))]
	   [(_)
	    (teach-syntax-error
	     'local
	     stx
	     #f
             "expected at least one definition (in square brackets) after local, but nothing's there")]
	   [_else (bad-use-error 'local stx)]))))











;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define (beginner)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define/proc first-order? assign? stx lambda-stx)

  (define (wrap-func-definition name argc k)
    (wrap-func-definitions first-order? 
                           '(procedure) (list name) (list argc)
                           (lambda (names)
                             (k (car names)))))

  (define (check-function-defn-ok stx)
    (when first-order?
      (when (eq? 'top-level (syntax-local-context))
        (teach-syntax-error
         'define
         stx
         #f
         "function definitions are not allowed in the interactions window; ~
              they must be in the definitions window"))))

  (unless (or (ok-definition-context)
              (identifier? stx))
    (teach-syntax-error
     'define
     stx
     #f
     "found a definition that is not at the top level"))

  (syntax-case stx ()
    ;; Constant or lambda def:
    [(_ name expr)
     (identifier? (syntax name))
     (let ([lam (syntax expr)])
       (check-defined-lambda lam)
       (syntax-case* (syntax expr) (beginner-lambda) (lambda (a b)
                                                       (free-identifier=? a lambda-stx))
                     ;; Well-formed lambda def:
                     [(beginner-lambda arg-seq lexpr ...)
                      (begin
                        (check-function-defn-ok stx)
                        (let-values ([(defn bind-names)
                                      (wrap-func-definition
                                       #'name
                                       (length (syntax->list #'arg-seq))
                                       (lambda (name)
                                         (with-syntax ([name name])
                                           (quasisyntax/loc 
                                               stx 
                                             (define name
                                               #,(stepper-syntax-property
                                                  (syntax-track-origin
                                                   #`(lambda arg-seq 
                                                       #,(stepper-syntax-property #`make-lambda-generative 
                                                                                  'stepper-skip-completely #t) 
                                                       lexpr ...)
                                                   lam
                                                   (syntax-local-introduce (car (syntax-e lam))))
                                                  'stepper-define-type
                                                  'lambda-define))))))])
                          (check-definition-new
                           'define
                           stx
                           #'name
                           defn
                           (and assign? bind-names))))]
                     ;; Constant def
                     [_else
                      (check-definition-new
                       'define
                       stx
                       (syntax name)
                       (quasisyntax/loc stx (define name expr))
                       (and assign? (list (syntax name))))]))]
    ;; Function definition:
    [(_ name-seq expr ...)
     (syntax-case (syntax name-seq) () [(name ...) #t][_else #f])
     ;; name-seq is at least a sequence
     (let ([names (syntax->list (syntax name-seq))])
       (check-function-defn-ok stx)
       (when (null? names)
         (teach-syntax-error
          'define
          stx
          #f
          "expected a name for the function, but nothing's there"))
       (let loop ([names names][pos 0])
         (unless (null? names)
           (unless (identifier? (car names))
             (teach-syntax-error
              'define
              stx
              (car names)
              "expected ~a, but found ~a"
              (cond
               [(zero? pos) "the name of the function"]
               [else "a variable"])
              (something-else/kw (car names))))
           (loop (cdr names) (add1 pos))))
       (when (null? (cdr names))
         (teach-syntax-error
          'define
          stx
          (syntax name-seq)
          "expected at least one variable after the function name, but found none"))
       (let ([dup (check-duplicate-identifier (cdr names))])
         (when dup
           (teach-syntax-error
            'define
            stx
            dup
            "found a variable that is used more than once: ~a"
            (syntax-e dup))))
       (check-single-result-expr (syntax->list (syntax (expr ...)))
                                 #f
                                 stx
                                 ;; can't local-expand function body, because
                                 ;;  not all top-level defns are ready:
                                 #f)
       
       (let-values ([(defn bind-names)
                     (wrap-func-definition
                      (car (syntax-e #'name-seq))
                      (length (cdr (syntax->list #'name-seq)))
                      (lambda (fn)
                        (with-syntax ([fn fn]
                                      [args (cdr (syntax-e #'name-seq))])
                          (quasisyntax/loc stx
                            (define fn
                              #,(stepper-syntax-property
                                 (stepper-syntax-property
                                  ;; this is so signature blame can report a
                                  ;; position for the procedure
                                  (syntax/loc stx (lambda args expr ...))
                                  'stepper-define-type
                                  'shortened-proc-define)
                                 'stepper-proc-define-name
                                 #`fn))))))])
         (check-definition-new 
          'define
          stx
          (car names)
          defn
          (and assign? bind-names))))]
    ;; Constant/lambda with too many or too few parts:
    [(_ name expr ...)
     (identifier? (syntax name))
     (let ([exprs (syntax->list (syntax (expr ...)))])
       (check-single-expression 'define
                                (format "after the variable name ~a"
                                        (syntax-e (syntax name)))
                                stx
                                exprs
                                ;; can't local-expand RHS, because
                                ;;  not all top-level defns are ready:
                                #f))]
    ;; Bad name/header:
    [(_ non-name expr ...)
     (teach-syntax-error
      'define
      stx
      (syntax non-name)
      "expected a variable name, or a function name and its variables (in parentheses), but found ~a"
      (something-else/kw (syntax non-name)))]
    ;; Missing name:
    [(_)
     (teach-syntax-error
      'define
      stx
      #f
      "expected a variable name, or a function name and its variables (in parentheses), but nothing's there")]
    [_else
     (bad-use-error 'define stx)]))


(define (wrap-func-definitions first-order? kinds names argcs k)
  (if first-order?
      (let ([name2s (map (make-name-inventer) names)])
        (values (quasisyntax
                 (begin
                   #,@(map
                       (lambda (name name2 kind argc)
                         #`(define-syntax #,name 
                             (make-first-order-function '#,kind 
                                                        #,argc
                                                        (quote-syntax #,name2) 
                                                        (quote-syntax #%app))))
                       names name2s kinds argcs)
                   #,(k name2s)))
                name2s))
      (values (k names)
              names)))

(define (ok-definition-context)
  (let ([ctx (syntax-local-context)])
    (or (memq ctx '(top-level module module-begin))
        (and (pair? ctx)
             (expanding-for-intermediate-local? (car ctx))))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define (advanced)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (advanced-define/proc stx)
  ;; Handle the case that doesn't fit into intermediate, then dispatch to 
  ;; the common code that it also used by beginner/intermediate.
  (syntax-case stx ()
    [(_ (name) expr)
     (and (identifier? (syntax name))
          (ok-definition-context))
     (check-definition-new
      'define
      stx
      (syntax name)
      (syntax/loc stx (define (name) expr))
      (list #'name))]
    [(_ (name) expr ...)
     (and (identifier? (syntax name))
          (ok-definition-context))
     (check-single-result-expr (syntax->list (syntax (expr ...)))
                               #f
                               stx
                               (list #'name))]
    [(_ . rest)
     ;; Call transformer define/proc.
     ;; Note that we call the transformer instead of producing
     ;; new syntax object that is an `intermediate-define' form;
     ;; that's important for syntax errors, so that they
     ;; report `advanced-define' as the source.
     (define/proc #f #t stx #'beginner-lambda)]
    [_else
     (bad-use-error 'define stx)]))







(define (advanced-lambda/proc stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_  (name ...) . exprs)
        (let ([names (syntax->list (syntax (name ...)))])
          (for-each (lambda (name)
                      (unless (identifier? name)
                        (teach-syntax-error
                         'lambda
                         stx
                         name
                         "expected a variable, but found ~a"
                         (something-else/kw name))))
                    names)
          (let ([dup (check-duplicate-identifier names)])
            (when dup
              (teach-syntax-error
               'lambda
               stx
               dup
               "found a variable that is used more than once: ~a"
               (syntax-e dup))))
          (check-single-expression 'lambda 
                                   "for the function body"
                                   stx
                                   (syntax->list (syntax exprs))
                                   names)
          (syntax/loc stx (lambda (name ...) . exprs)))]
       [(_ arg-non-seq . exprs)
        (teach-syntax-error
         'lambda
         stx
         (syntax arg-non-seq)
         "expected at least one variable (in parentheses) after lambda, but found ~a"
         (something-else (syntax arg-non-seq)))]
       [(_)
        (teach-syntax-error
         'lambda
         stx
         #f
         "expected at least one variable (in parentheses) after lambda, but nothing's there")]
       [_else
        (bad-use-error 'lambda stx)]))))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; when and unless (advanced)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (advanced-when/proc advanced-unless/proc)
  (let ([mk
         (lambda (who target-stx)
           (lambda (stx)
             (ensure-expression
              stx
              (lambda ()
                (syntax-case stx ()
                  [(_)
                   (teach-syntax-error
                    who
                    stx
                    #f
                    "expected a question and an answer, but nothing's there")]
                  [(_ q)
                   (teach-syntax-error
                    who
                    stx
                    #'q
                    "expected a question and an answer, but found only one part")]		      
                  [(_ q a)
                   (with-syntax ([who who]
                                 [target target-stx])
                     (syntax/loc stx (target (verify-boolean q 'who) a)))]
                  [(_ . parts)
                   (teach-syntax-error*
                    who
                    stx
                    (syntax->list #'parts)
                    "expected a question and an answer, but found ~a parts" (length (syntax->list #'parts)))]
                  [_else
                   (bad-use-error who stx)])))))])
    (values (mk 'when (quote-syntax when))
            (mk 'unless (quote-syntax unless)))))












;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set! (advanced)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We disallow set!s on lambda-bound variables, which we recognize
;; as lexically-bound variables that are not bound to
;; set!-transformer syntax values.

(define-values (advanced-set!/proc advanced-set!-continue/proc)
  (let ([proc
         (lambda (continuing?)
           (lambda (stx)
             (ensure-expression
              stx
              (lambda ()
                (syntax-case stx ()
                  [(_ id expr ...)
                   (identifier? (syntax id))
                   (let ([exprs (syntax->list (syntax (expr ...)))])
                     ;; Check that id isn't syntax, and not lexical.
                     (when ((with-handlers ([exn:fail? (lambda (exn) (lambda () #t))])
                              ;; First try syntax:
                              (let ([binding (syntax-local-value (syntax id))])
                                ;; If it's a transformer binding, then it can take care of itself...
                                (if (set!-transformer? binding)
                                    (lambda () #f) ;; no lex check wanted
                                    (lambda ()
                                      (teach-syntax-error
                                       'set!
                                       stx
                                       (syntax id)
                                       "expected a variable after set!, but found a ~a" (syntax-e #'id)))))))
                       ;; Now try lexical:
                       (when (eq? 'lexical (identifier-binding (syntax id)))
                         (teach-syntax-error
                          'set!
                          stx
                          (syntax id)
                          "expected a mutable variable after set!, but found a variable that cannot be modified: ~a"
                          (syntax-e #'id))))
                     ;; If we're in a module, we'd like to check here whether
                     ;;  the identier is bound, but we need to delay that check
                     ;;  in case the id is defined later in the module. So only
                     ;;  do this in continuing mode:
                     (when continuing?
                       (let ([binding (identifier-binding #'id)])
                         (cond
                          [(and (not binding)
                                (syntax-source-module #'id))
                           (teach-syntax-error
                            #f
                            #'id
                            #f
                            "this variable is not defined")]
                          [(and (list? binding)
                                (or (not (module-path-index? (car binding)))
                                    (let-values ([(path rel) (module-path-index-split (car binding))])
                                      path)))
                           (teach-syntax-error
                            'set!
                            #'id
                            #f
                            "expected a mutable variable after set!, but found a variable that cannot be modified: ~a"
                            (syntax-e #'id))])))
                     ;; Check the RHS
                     (check-single-expression 'set!
                                              "for the new value"
                                              stx
                                              exprs
                                              null)

                     (if continuing?
                         (stepper-syntax-property
                          (quasisyntax/loc stx (begin #,(datum->syntax #'here `(set! ,#'id ,@(syntax->list #'(expr ...))) stx) set!-result))
                          'stepper-skipto
                          (append skipto/cdr
                                  skipto/first))
                         (stepper-ignore-checker (quasisyntax/loc stx (#%app values #,(advanced-set!-continue/proc
                                                                                       (syntax/loc stx (_ id expr ...))))))))]
                  [(_ id . __)
                   (teach-syntax-error
                    'set!
                    stx
                    (syntax id)
                    "expected a variable after set!, but found ~a"
                    (something-else (syntax id)))]
                  [(_)
                   (teach-syntax-error
                    'set!
                    stx
                    #f
                    "expected a variable after set!, but nothing's there")]
                  [_else (bad-use-error 'set! stx)])))))])
    (values (proc #f)
            (proc #t))))






;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (advanced-case/proc stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_)
        (teach-syntax-error
         'case
         stx
         #f
         "expected an expression after case, but nothing's there")]
       [(_ expr)
        (teach-syntax-error
         'case
         stx
         #f
         "expected a clause with at least one choice (in parentheses) and an answer after the expression, but nothing's there")]
       [(_ v-expr clause ...)
        (let ([clauses (syntax->list (syntax (clause ...)))])
          (for-each
           (lambda (clause)
             (syntax-case clause (beginner-else)
               [(beginner-else answer ...)
                (let ([lpos (memq clause clauses)])
                  (when (not (null? (cdr lpos)))
                    (teach-syntax-error
                     'case
                     stx
                     clause
                     "found an else clause that isn't the last clause ~
                                    in its case expression"))
                  (let ([answers (syntax->list (syntax (answer ...)))])
                    (check-single-expression 'case
                                             "for the answer in the case clause"
                                             clause
                                             answers
                                             null)))]
               [(choices answer ...)
                (let ([choices (syntax choices)]
                      [answers (syntax->list (syntax (answer ...)))])
                  (syntax-case choices ()
                    [(elem ...)
                     (let ([elems (syntax->list (syntax (elem ...)))])
                       (for-each (lambda (e)
                                   (let ([v (syntax-e e)])
                                     (unless (or (number? v)
                                                 (symbol? v))
                                       (teach-syntax-error
                                        'case
                                        stx
                                        e
                                        "expected a symbol (without its quote) or a number as a choice, but found ~a"
                                        (something-else e)))))
                                 elems))]
                    [_else (teach-syntax-error
                            'case
                            stx
                            choices
                            "expected at least one choice (in parentheses), but found ~a"
                            (something-else choices))])
                  (when (stx-null? choices)
                    (teach-syntax-error
                     'case
                     stx
                     choices
                     "expected a symbol (without its quote) or a number as a choice, but nothing's there"))
                  (check-single-expression 'case
                                           "for the answer in the case clause"
                                           clause
                                           answers
                                           null))]
               [()
                (teach-syntax-error
                 'case
                 stx
                 clause
                 "expected a clause with at least one choice (in parentheses) and an answer, but found an empty part")]
               [_else
                (teach-syntax-error
                 'case
                 stx
                 clause
                 "expected a clause with at least one choice (in parentheses) and an answer, but found ~a"
                 (something-else clause))]))
           clauses)
          ;; Add `else' clause for error, if necessary:
          (let ([clauses (let loop ([clauses clauses])
                           (cond
                            [(null? clauses)
                             (list
                              (syntax/loc stx
                                [else (error 'cases "the expression matched none of the choices")]))]
                            [(syntax-case (car clauses) (beginner-else)
                               [(beginner-else . _) (syntax/loc (car clauses) (else . _))]
                               [_else #f])
                             => 
                             (lambda (x) (cons x (cdr clauses)))]
                            [else (cons (car clauses) (loop (cdr clauses)))]))])
            (with-syntax ([clauses clauses])
              (syntax/loc stx (case v-expr . clauses)))))]
       [_else (bad-use-error 'case stx)]))))
