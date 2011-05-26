#lang typed/racket/base

(define-type RelativeString String)
(define-type UserString String)
(define-type PackageString String)




(define-type ModulePath (U (List 'quote Symbol)
                           RelativeString
                           (Pairof 'lib (Pairof RelativeString (Listof RelativeString)))
                           Symbol
                           (List 'file String)
                           (List 'planet Symbol)
                           (List 'planet String)
                           (Pairof 'planet 
                                   (Pairof RelativeString
                                           (Pairof (U (List UserString PackageString)
                                                      (List UserString PackageString Natural)
                                                      (List UserString PackageString Natural MinorVersion))
                                                   (Listof RelativeString))))))
                                            

(define-type MinorVersion (U Natural
                             (List Natural Natural)
                             (List '= Natural)
                             (List '+ Natural)
                             (List '- Natural)))


(require/typed racket/base                       
               
               [opaque ModulePathIndex module-path-index?]
               [opaque ResolvedModulePath resolved-module-path?]
                        
               [module-path-index-resolve 
                (ModulePathIndex -> ResolvedModulePath)]
               
               [module-path-index-join
                ((U ModulePath #f)
                 (U ModulePathIndex ResolvedModulePath #f) ->
                 ModulePathIndex)]
               
               [module-path-index-split 
                (ModulePathIndex -> (values (U ModulePath #f)
                                            (U ModulePathIndex ResolvedModulePath #f)))]
               
               [resolved-module-path-name
                (ResolvedModulePath -> (U Path Symbol))]
               [make-resolved-module-path ((U Symbol Path) -> ResolvedModulePath)])
                                            


(provide 
 
 ModulePath
 ResolvedModulePath
         
 ModulePathIndex
 module-path-index-resolve
 module-path-index-join
 module-path-index-split)
