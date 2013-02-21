#lang racket/base

;; Implements a button with alternatives.

(require racket/class
         racket/list
         mrlib/name-message
         framework)

(provide button-with-alternatives%)



;; Most of this is stolen from the custom controls written in
;; drracket/private/unit.rkt.  It might be good to generalize this
;; so it's easier to use.
(define button-with-alternatives%
  (class name-message%
    (init-field parent)
    (init-field choices-thunk)

    (define currently-selected 
      (let ([choices (choices-thunk)])
        (cond
          [(empty? choices)
           #f]
          [else
           (first (choices-thunk))])))
    
    (define/public (get-selection)
      currently-selected)
    
    (define/public (get-choices)
      (choices-thunk))
    
    (define/override (fill-popup menu reset)
      (for ([ch (choices-thunk)])
        (make-menu-item menu ch)))

    (define (make-menu-item menu ch)
      (define item
        (new (if (and currently-selected
                      (string=? ch currently-selected))
                 menu:can-restore-checkable-menu-item%
                 menu:can-restore-menu-item%)
             [label (gui-utils:quote-literal-label ch)]
             [parent menu]
             [callback (lambda (menu-item control-event)
                         (set! currently-selected ch))]))
      (when (string=? ch currently-selected)
        (send item check #t))
      item)
      
    (super-new [parent parent]
               [label ""])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

