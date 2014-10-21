#lang whalesong (require "../selfhost-lang.rkt")
(require whalesong/lang/for)
; #lang typed/racket/base
(require "assemble-helpers.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/expression-structs.rkt"
	 "../compiler/lexical-structs.rkt"
         "../compiler/kernel-primitives.rkt"
         "../parameters.rkt"
         "assemble-structs.rkt"
	 racket/string)

(provide assemble-op-statement)



(: assemble-op-statement (PrimitiveCommand Blockht -> String))
(define (assemble-op-statement op blockht)  
  (cond 
    
    [(CheckToplevelBound!? op)
     (format "if (M.e[M.e.length-~a][~a]===void(0)){ RT.raiseUnboundToplevelError(M,M.e[M.e.length-~a].names[~a]); }"
             (add1 (CheckToplevelBound!-depth op))
             (CheckToplevelBound!-pos op)
             (add1 (CheckToplevelBound!-depth op))
             (CheckToplevelBound!-pos op))]
    [(CheckGlobalBound!? op)
     (format "if (M.globals[~s]===void(0)&&M.params.currentNamespace.get(~s)===void(0)){ RT.raiseUnboundToplevelError(M,~s); }"
             (symbol->string (CheckGlobalBound!-name op))
             (symbol->string (CheckGlobalBound!-name op))
             (symbol->string (CheckGlobalBound!-name op)))]


    [(CheckClosureAndArity!? op)
     "RT.checkClosureAndArity(M);"]
    
    [(CheckPrimitiveArity!? op)
     "RT.checkPrimitiveArity(M);"]

    [(ExtendEnvironment/Prefix!? op)
     (let (; : (Listof (U Symbol False GlobalBucket ModuleVariable))
           [names (ExtendEnvironment/Prefix!-names op)]) 
           (format "M.e.push([~a]);M.e[M.e.length-1].names=[~a];"
                   (string-join (map
                                 (lambda (n) ; ([n : (U Symbol False GlobalBucket ModuleVariable)])
                                          (cond [(symbol? n)
                                                 (format "M.params.currentNamespace.get(~s)||M.primitives[~s]"
                                                         (symbol->string n) 
                                                         (symbol->string n))]
                                                [(eq? n #f)
                                                 "false"]
                                                [(GlobalBucket? n)
                                                 (format "M.globals[~s]!==void(0)?M.globals[~s]:M.params.currentNamespace.get(~s)"
                                                         (symbol->string (GlobalBucket-name n))
                                                         (symbol->string (GlobalBucket-name n))
                                                         (symbol->string (GlobalBucket-name n)))]
                                                ;; FIXME:  this should be looking at the module path and getting
                                                ;; the value here!  It shouldn't be looking into Primitives...
                                                [(ModuleVariable? n)
                                                 (cond
                                                  [(kernel-module-name? (ModuleVariable-module-name n))
                                                   (format "M.primitives[~s]"
                                                           (symbol->string
                                                            (kernel-module-variable->primitive-name n)))]
                                                  [else
                                                   (define module-name
                                                     (symbol->string
                                                            (ModuleLocator-name
                                                             (ModuleVariable-module-name n))))
                                                   (format "[M.modules[~s].prefix,M.modules[~s].getPrefixOffset(~s),{moduleName:~s,name:~s}]"
                                                           module-name
                                                           module-name
                                                           (symbol->string (ModuleVariable-name n))
                                                           module-name
                                                           (symbol->string (ModuleVariable-name n)))])]))
                                 names)
                                ",")
                   (string-join (map
				 (lambda (n) ; ([n : (U Symbol False GlobalBucket ModuleVariable)])
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
     (format "M.e.push(~a);"
             (string-join (build-list (InstallClosureValues!-n op)
                                      (lambda (i) ; ([i : Natural])
                                               (format "M.p.closedVals[~a]" i)))
                          ","))]
    
    [(RestoreEnvironment!? op)
     "M.e=M.e[M.e.length-2].slice(0);"]
    
    [(RestoreControl!? op)
     (format "M.restoreControl(~a);"
             (let ([tag ;  : (U DefaultContinuationPromptTag OpArg)
			 (RestoreControl!-tag op)])
               (cond
                 [(DefaultContinuationPromptTag? tag)
                  (assemble-default-continuation-prompt-tag)]
                 [(OpArg? tag)
                  (assemble-oparg tag blockht)])))]
    
    [(FixClosureShellMap!? op)
     (format "M.e[M.e.length-~a].closedVals=[~a];"
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
     (format "M.c[M.c.length-1].p=~a;"
             (assemble-oparg (SetFrameCallee!-proc op)
                             blockht))]
    
    [(SpliceListIntoStack!? op)
     (format "M.spliceListIntoStack(~a);"
             (assemble-oparg (SpliceListIntoStack!-depth op)
                             blockht))]

    [(UnspliceRestFromStack!? op)
     (format "M.unspliceRestFromStack(~a,~a);"
             (assemble-oparg (UnspliceRestFromStack!-depth op) blockht)
             (assemble-oparg (UnspliceRestFromStack!-length op) blockht))]

    [(InstallContinuationMarkEntry!? op)
     (string-append "M.installContinuationMarkEntry("
                    "M.c[M.c.length-1].pendingContinuationMarkKey,"
                    "M.v);")]

    [(RaiseContextExpectedValuesError!? op)
     (format "RT.raiseContextExpectedValuesError(M,~a);"
             (RaiseContextExpectedValuesError!-expected op))]


    [(RaiseArityMismatchError!? op)
     (format "RT.raiseArityMismatchError(M,~a,~a);"
             (assemble-oparg (RaiseArityMismatchError!-proc op) blockht)
             (assemble-oparg (RaiseArityMismatchError!-received op) blockht))]


    [(RaiseOperatorApplicationError!? op)
     (format "RT.raiseOperatorApplicationError(M,~a);"
             (assemble-oparg (RaiseOperatorApplicationError!-operator op) blockht))]


    [(RaiseUnimplementedPrimitiveError!? op)
     (format "RT.raiseUnimplementedPrimitiveError(M,~s);"
             (symbol->string (RaiseUnimplementedPrimitiveError!-name op)))]
    
    [(LinkModule!? op)
     (format "RT.PAUSE(
                  function(restart){
                      var modname = ~s;
                      RT.currentModuleLoader(M,modname,
                                             function(){
                                                 M.modules[modname] = M.installedModules[modname]();
                                                 restart(~a);
                                             },
                                             function(){
                                                 RT.raiseModuleLoadingError(M,modname); 
                                             });
                  });"
             (symbol->string (ModuleLocator-name (LinkModule!-path op)))
             (assemble-label (make-Label (LinkModule!-label op))))]
    
    [(InstallModuleEntry!? op)
     (format "M.installedModules[~s]=function(){return new RT.ModuleRecord(~s,~a);}"
             (symbol->string (ModuleLocator-name (InstallModuleEntry!-path op)))
             (symbol->string (InstallModuleEntry!-name op))
             (assemble-label (make-Label (InstallModuleEntry!-entry-point op))))]

    [(MarkModuleInvoked!? op)
     (format "M.modules[~s].isInvoked=true;"
             (symbol->string (ModuleLocator-name (MarkModuleInvoked!-path op))))]


    [(AliasModuleAsMain!? op)
     (format "M.mainModules.push(~s);"
             (symbol->string (ModuleLocator-name (AliasModuleAsMain!-from op))))]

    [(FinalizeModuleInvokation!? op)
     (define modname (symbol->string (ModuleLocator-name (FinalizeModuleInvokation!-path op))))
     (string-append
      "(function (selfMod,ns,extNs,prefix) {"
      (string-join (for/list ([a-provide (FinalizeModuleInvokation!-provides op)])
                     (cond [(kernel-module-name? (ModuleProvide-source a-provide))
                            (format "ns.set(~s, M.primitives[~s]);"
                                    (symbol->string (ModuleProvide-external-name a-provide))
                                    (symbol->string (ModuleProvide-internal-name a-provide)))]
                           [(equal? (ModuleLocator-name (ModuleProvide-source a-provide))
                                    (ModuleLocator-name (FinalizeModuleInvokation!-path op)))
                            (string-append (format "ns.set(~s, prefix[selfMod.getPrefixOffset(~s)]);"
                                                   (symbol->string (ModuleProvide-internal-name a-provide))
                                                   (symbol->string (ModuleProvide-internal-name a-provide)))
                                           ;; For JS-derived code, it might be inconvenient to get the bindings by internal
                                           ;; name.  We assign a separate mapping here to make it easier to access.
                                           (format "extNs.set(~s, prefix[selfMod.getPrefixOffset(~s)]);"
                                                   (symbol->string (ModuleProvide-external-name a-provide))
                                                   (symbol->string (ModuleProvide-internal-name a-provide))))]
                           [else
                            (format "ns.set(~s, M.modules[~s].getExports().get(~s));" 
                                    (symbol->string (ModuleProvide-external-name a-provide))
                                    (symbol->string (ModuleLocator-name (ModuleProvide-source a-provide)))
                                    (symbol->string (ModuleProvide-internal-name a-provide)))]))
                   "")
      (format "}(M.modules[~s],M.modules[~s].getExports(),M.modules[~s].getExternalExports(),M.modules[~s].prefix));" modname modname modname modname))]))
