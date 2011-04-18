#lang typed/racket/base
(require "il-structs.rkt"
	 "assemble-helpers.rkt"
	 "lexical-structs.rkt"
	 racket/string)

(provide assemble-op-statement)



(: assemble-op-statement (PrimitiveCommand -> String))
(define (assemble-op-statement op)  
  (cond 
    
    [(CheckToplevelBound!? op)
     (format "if (MACHINE.env[MACHINE.env.length - 1 - ~a][~a] === undefined) { throw new Error(\"Not bound: \" + MACHINE.env[MACHINE.env.length - 1 - ~a].names[~a]); }"
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op)
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op))]
    
    [(CheckClosureArity!? op)
     (format "if (! (MACHINE.proc instanceof RUNTIME.Closure && RUNTIME.isArityMatching(MACHINE.proc.arity, ~a))) { if (! (MACHINE.proc instanceof RUNTIME.Closure)) { throw new Error(\"not a closure\"); } else { throw new Error(\"arity failure:\" + MACHINE.proc.displayName); } }"
             (assemble-oparg (CheckClosureArity!-arity op)))]
    
    [(CheckPrimitiveArity!? op)
     (format "if (! (typeof(MACHINE.proc) === 'function'  && RUNTIME.isArityMatching(MACHINE.proc.arity, ~a))) { if (! (typeof(MACHINE.proc) === 'function')) { throw new Error(\"not a primitive procedure\"); } else { throw new Error(\"arity failure:\" + MACHINE.proc.displayName); } }"
             (assemble-oparg (CheckPrimitiveArity!-arity op)))]
     
    
    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof (U Symbol False ModuleVariable)) (ExtendEnvironment/Prefix!-names op)])
           (format "MACHINE.env.push([~a]);  MACHINE.env[MACHINE.env.length-1].names = [~a];"
                   (string-join (map
				 (lambda: ([n : (U Symbol False ModuleVariable)])
					  (cond [(symbol? n)
						 (format "MACHINE.params.currentNamespace[~s] || MACHINE.primitives[~s]"
							 (symbol->string n) 
							 (symbol->string n))]
						[(eq? n #f)
						 "false"]
						[(ModuleVariable? n)
						 (format "MACHINE.primitives[~s]"
							 (symbol->string (ModuleVariable-name n)))]))
				 names)
                                ",")
                   (string-join (map
				 (lambda: ([n : (U Symbol False ModuleVariable)])
					  (cond
					   [(symbol? n)
					    (format "~s" (symbol->string n))]
					   [(eq? n #f)
					    "false"]
					   [(ModuleVariable? n)
					    (format "~s" (symbol->string (ModuleVariable-name n)))]))
				 names)
                                ",")))]
    
    [(InstallClosureValues!? op)
     "MACHINE.env.splice.apply(MACHINE.env, [MACHINE.env.length, 0].concat(MACHINE.proc.closedVals));"]
    [(RestoreEnvironment!? op)
     "MACHINE.env = MACHINE.env[MACHINE.env.length - 2].slice(0);"]
    [(RestoreControl!? op)
     (format "RUNTIME.restoreControl(MACHINE, ~a);"
             (let: ([tag : (U DefaultContinuationPromptTag OpArg)
			 (RestoreControl!-tag op)])
               (cond
                 [(DefaultContinuationPromptTag? tag)
                  (assemble-default-continuation-prompt-tag)]
                 [(OpArg? tag)
                  (assemble-oparg tag)])))]
    [(FixClosureShellMap!? op)
     (format "MACHINE.env[MACHINE.env.length - 1 - ~a].closedVals = [~a]"
             (FixClosureShellMap!-depth op)
              (string-join (map
			    assemble-env-reference/closure-capture 
			    ;; The closure values are in reverse order
			    ;; to make it easier to push, in bulk, into
			    ;; the environment (which is also in reversed order)
			    ;; during install-closure-values.
			    (reverse (FixClosureShellMap!-closed-vals op)))
			   ", "))]
    [(SetFrameCallee!? op)
     (format "MACHINE.control[MACHINE.control.length-1].proc = ~a;"
             (assemble-oparg (SetFrameCallee!-proc op)))]
    [(SpliceListIntoStack!? op)
     (format "RUNTIME.spliceListIntoStack(MACHINE, ~a);"
             (assemble-oparg (SpliceListIntoStack!-depth op)))]

    [(UnspliceRestFromStack!? op)
     (format "RUNTIME.unspliceRestFromStack(MACHINE, ~a, ~a);"
             (assemble-oparg (UnspliceRestFromStack!-depth op))
             (assemble-oparg (UnspliceRestFromStack!-length op)))]

    [(InstallContinuationMarkEntry!? op)
     (string-append "RUNTIME.installContinuationMarkEntry(MACHINE,"
                    "MACHINE.control[MACHINE.control.length-1].pendingContinuationMarkKey,"
                    "MACHINE.val);")]

    [(RaiseContextExpectedValuesError!? op)
     (format "RUNTIME.raiseContextExpectedValuesError(MACHINE, ~a);"
             (RaiseContextExpectedValuesError!-expected op))]


    [(RaiseArityMismatchError!? op)
     (format "RUNTIME.raiseArityMismatchError(MACHINE, ~a, ~a);"
             (assemble-arity (RaiseArityMismatchError!-expected op))
             (assemble-oparg (RaiseArityMismatchError!-received op)))]


    [(RaiseOperatorApplicationError!? op)
     (format "RUNTIME.raiseOperatorApplicationError(MACHINE, ~a);"
             (assemble-oparg (RaiseOperatorApplicationError!-operator op)))]))
