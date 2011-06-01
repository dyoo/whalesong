#lang typed/racket/base
(require "assemble-helpers.rkt"
         "../compiler/il-structs.rkt"
	 "../compiler/lexical-structs.rkt"
         "../parameters.rkt"
	 racket/string)

(provide assemble-op-statement)



(: assemble-op-statement (PrimitiveCommand -> String))
(define (assemble-op-statement op)  
  (cond 
    
    [(CheckToplevelBound!? op)
     (format "if (MACHINE.env[MACHINE.env.length - 1 - ~a][~a] === undefined) { RUNTIME.raiseUnboundToplevelError(MACHINE.env[MACHINE.env.length - 1 - ~a].names[~a]); }"
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op)
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op))]


    [(CheckClosureArity!? op)
     (format #<<EOF
              if (! (MACHINE.proc instanceof RUNTIME.Closure)) {
                  RUNTIME.raiseOperatorIsNotClosure(MACHINE, MACHINE.proc);
              }
              if (! RUNTIME.isArityMatching(MACHINE.proc.arity, ~a)) {
                  RUNTIME.raiseArityMismatchError(MACHINE.proc,
                                                  MACHINE.proc.arity,
                                                  ~a);
              }
EOF
             (assemble-oparg (CheckClosureArity!-arity op))
             (assemble-oparg (CheckClosureArity!-arity op)))]

    
    [(CheckPrimitiveArity!? op)
     (format #<<EOF
              if (! (typeof(MACHINE.proc) === 'function')) {
                  RUNTIME.raiseOperatorIsNotPrimitiveProcedure(MACHINE, MACHINE.proc);
              }
              if (! RUNTIME.isArityMatching(MACHINE.proc.arity, ~a)) {
                  RUNTIME.raiseArityMismatchError(MACHINE.proc,
                                                  MACHINE.proc.arity,
                                                  ~a);
              }
EOF
             (assemble-oparg (CheckPrimitiveArity!-arity op))
             (assemble-oparg (CheckPrimitiveArity!-arity op)))]
     
    
    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof (U Symbol False GlobalBucket ModuleVariable)) (ExtendEnvironment/Prefix!-names op)])
           (format "MACHINE.env.push([~a]);  MACHINE.env[MACHINE.env.length-1].names = [~a];"
                   (string-join (map
                                 (lambda: ([n : (U Symbol False GlobalBucket ModuleVariable)])
                                          (cond [(symbol? n)
                                                 (format "MACHINE.params.currentNamespace[~s] || MACHINE.primitives[~s]"
                                                         (symbol->string n) 
                                                         (symbol->string n))]
                                                [(eq? n #f)
                                                 "false"]
                                                [(GlobalBucket? n)
                                                 ;; FIXME: maybe we should keep a set of global variables here?
                                                 (format "MACHINE.primitives[~s]"
                                                         (symbol->string (GlobalBucket-name n)))]
                                                ;; FIXME:  this should be looking at the module path and getting
                                                ;; the value here!  It shouldn't be looking into Primitives...
                                                [(ModuleVariable? n)
                                                 (cond
                                                  [((current-kernel-module-locator?)
                                                    (ModuleVariable-module-name n))
                                                   (format "MACHINE.primitives[~s]"
                                                           (symbol->string (ModuleVariable-name n)))]
                                                  [else
                                                   (format "MACHINE.modules[~s].namespace[~s]"
                                                           (symbol->string
                                                            (ModuleLocator-name
                                                             (ModuleVariable-module-name n)))
                                                           (symbol->string (ModuleVariable-name n)))])]))
                                 names)
                                ",")
                   (string-join (map
				 (lambda: ([n : (U Symbol False GlobalBucket ModuleVariable)])
					  (cond
					   [(symbol? n)
					    (format "~s" (symbol->string n))]
					   [(eq? n #f)
					    "false"]
                                           [(GlobalBucket? n)
                                            (format "~s" (symbol->string (GlobalBucket-name n)))]
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
     (format "RUNTIME.raiseArityMismatchError(MACHINE, ~a, ~a, ~a);"
             (assemble-oparg (RaiseArityMismatchError!-proc op))
             (assemble-arity (RaiseArityMismatchError!-expected op))
             (assemble-oparg (RaiseArityMismatchError!-received op)))]


    [(RaiseOperatorApplicationError!? op)
     (format "RUNTIME.raiseOperatorApplicationError(MACHINE, ~a);"
             (assemble-oparg (RaiseOperatorApplicationError!-operator op)))]


    [(RaiseUnimplementedPrimitiveError!? op)
     (format "RUNTIME.raiseUnimplementedPrimitiveError(MACHINE, ~s);"
             (symbol->string (RaiseUnimplementedPrimitiveError!-name op)))]
    
    
    [(InstallModuleEntry!? op)
     (format "MACHINE.modules[~s]=new RUNTIME.ModuleRecord(~s, ~a);"
             (symbol->string (ModuleLocator-name (InstallModuleEntry!-path op)))
             (symbol->string (InstallModuleEntry!-name op))
             (assemble-label (make-Label (InstallModuleEntry!-entry-point op))))]

    [(MarkModuleInvoked!? op)
     (format "MACHINE.modules[~s].isInvoked = true;"
             (symbol->string (ModuleLocator-name (MarkModuleInvoked!-path op))))]


    [(AliasModuleAsMain!? op)
     (format "MACHINE.mainModules.push(MACHINE.modules[~s]);"
             (symbol->string (ModuleLocator-name (AliasModuleAsMain!-from op))))]

    [(FinalizeModuleInvokation!? op)
     (format "MACHINE.modules[~s].finalizeModuleInvokation();"
             (symbol->string
              (ModuleLocator-name (FinalizeModuleInvokation!-path op))))]))
