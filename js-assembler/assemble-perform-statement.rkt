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
     (format "if (M.env[M.env.length - 1 - ~a][~a] === undefined) { RT.raiseUnboundToplevelError(M.env[M.env.length - 1 - ~a].names[~a]); }"
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op)
             (CheckToplevelBound!-depth op)
             (CheckToplevelBound!-pos op))]


    [(CheckClosureAndArity!? op)
     (format "RT.checkClosureAndArity(M, ~a);\n"
             (assemble-oparg (CheckClosureAndArity!-num-args op)))]

    [(ExtendEnvironment/Prefix!? op)
     (let: ([names : (Listof (U Symbol False GlobalBucket ModuleVariable)) (ExtendEnvironment/Prefix!-names op)])
           (format "M.env.push([~a]);M.env[M.env.length-1].names=[~a];"
                   (string-join (map
                                 (lambda: ([n : (U Symbol False GlobalBucket ModuleVariable)])
                                          (cond [(symbol? n)
                                                 (format "M.params.currentNamespace[~s] || M.primitives[~s]"
                                                         (symbol->string n) 
                                                         (symbol->string n))]
                                                [(eq? n #f)
                                                 "false"]
                                                [(GlobalBucket? n)
                                                 ;; FIXME: maybe we should keep a set of global variables here?
                                                 (format "M.primitives[~s]"
                                                         (symbol->string (GlobalBucket-name n)))]
                                                ;; FIXME:  this should be looking at the module path and getting
                                                ;; the value here!  It shouldn't be looking into Primitives...
                                                [(ModuleVariable? n)
                                                 (cond
                                                  [((current-kernel-module-locator?)
                                                    (ModuleVariable-module-name n))
                                                   (format "M.primitives[~s]"
                                                           (symbol->string (ModuleVariable-name n)))]
                                                  [else
                                                   (format "M.modules[~s].namespace[~s]"
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
     "M.env.push.apply(M.env,M.proc.closedVals);"]
    
    [(RestoreEnvironment!? op)
     "M.env=M.env[M.env.length-2].slice(0);"]
    
    [(RestoreControl!? op)
     (format "M.restoreControl(~a);"
             (let: ([tag : (U DefaultContinuationPromptTag OpArg)
			 (RestoreControl!-tag op)])
               (cond
                 [(DefaultContinuationPromptTag? tag)
                  (assemble-default-continuation-prompt-tag)]
                 [(OpArg? tag)
                  (assemble-oparg tag)])))]
    
    [(FixClosureShellMap!? op)
     (format "M.env[M.env.length-~a].closedVals=[~a];"
             (add1 (FixClosureShellMap!-depth op))
             (string-join (map
                           assemble-env-reference/closure-capture 
                           ;; The closure values are in reverse order
                           ;; to make it easier to push, in bulk, into
                           ;; the environment (which is also in reversed order)
                           ;; during install-closure-values.
                           (reverse (FixClosureShellMap!-closed-vals op)))
                          ","))]
    
    [(SetFrameCallee!? op)
     (format "M.control[M.control.length-1].proc = ~a;"
             (assemble-oparg (SetFrameCallee!-proc op)))]
    
    [(SpliceListIntoStack!? op)
     (format "M.spliceListIntoStack(~a);"
             (assemble-oparg (SpliceListIntoStack!-depth op)))]

    [(UnspliceRestFromStack!? op)
     (format "M.unspliceRestFromStack(~a,~a);"
             (assemble-oparg (UnspliceRestFromStack!-depth op))
             (assemble-oparg (UnspliceRestFromStack!-length op)))]

    [(InstallContinuationMarkEntry!? op)
     (string-append "M.installContinuationMarkEntry("
                    "M.control[M.control.length-1].pendingContinuationMarkKey,"
                    "M.val);")]

    [(RaiseContextExpectedValuesError!? op)
     (format "RT.raiseContextExpectedValuesError(M,~a);"
             (RaiseContextExpectedValuesError!-expected op))]


    [(RaiseArityMismatchError!? op)
     (format "RT.raiseArityMismatchError(M,~a,~a);"
             (assemble-oparg (RaiseArityMismatchError!-proc op))
             (assemble-oparg (RaiseArityMismatchError!-received op)))]


    [(RaiseOperatorApplicationError!? op)
     (format "RT.raiseOperatorApplicationError(M,~a);"
             (assemble-oparg (RaiseOperatorApplicationError!-operator op)))]


    [(RaiseUnimplementedPrimitiveError!? op)
     (format "RT.raiseUnimplementedPrimitiveError(M,~s);"
             (symbol->string (RaiseUnimplementedPrimitiveError!-name op)))]
    
    
    [(InstallModuleEntry!? op)
     (format "M.modules[~s]=new RT.ModuleRecord(~s,~a);"
             (symbol->string (ModuleLocator-name (InstallModuleEntry!-path op)))
             (symbol->string (InstallModuleEntry!-name op))
             (assemble-label (make-Label (InstallModuleEntry!-entry-point op))))]

    [(MarkModuleInvoked!? op)
     (format "M.modules[~s].isInvoked=true;"
             (symbol->string (ModuleLocator-name (MarkModuleInvoked!-path op))))]


    [(AliasModuleAsMain!? op)
     (format "M.mainModules.push(M.modules[~s]);"
             (symbol->string (ModuleLocator-name (AliasModuleAsMain!-from op))))]

    [(FinalizeModuleInvokation!? op)
     (format "M.modules[~s].finalizeModuleInvokation();"
             (symbol->string
              (ModuleLocator-name (FinalizeModuleInvokation!-path op))))]))
