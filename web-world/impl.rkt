#lang s-exp "../lang/js/js.rkt"


;; Make sure the resource library is loaded.
(require "../resource.rkt")


(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-tree-cursor.js"
               "js-impl.js")
 #:provided-values (big-bang

                    ;; initial view 
                    initial-view

                    ;; stop-when handler
                    stop-when

                    ;; clock tick handler
                    on-tick

                    ;; draw and update the view
                    to-draw
                    
                    ;; coerse to view
                    ->view

                    view-focus
                    view-text
                    update-view-text
                    ))
