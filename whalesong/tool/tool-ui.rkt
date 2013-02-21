#lang racket
(require "button-with-alternatives.rkt"
         racket/gui/base)

;; Defines the Whalesong tool user interface.  We add a button
;; with choices to either run the program in the browser, or
;; build a package.


(define (whalesong-tool-ui parent-widget
                           #:label (label "Run Whalesong")
                           #:on-browser (on-browser
                                         (lambda ()
                                           (void)))
                           #:on-build-package (on-build-package 
                                               (lambda ()
                                                 (void))))
  (define container (new horizontal-pane%
                         [parent parent-widget]))
  
  (define b 
    (new button% 
         [label label]
         [callback (lambda (b ce)
                     (define selection 
                       (send alternatives get-selection))
                     (cond
                       [(string=? selection "Run in browser")
                        (on-browser)]
                       [(string=? selection "Build smartphone package")
                        (on-build-package)]
                       [else
                        (void)]))]
         [parent container]))  
  
  (define alternatives
    (new button-with-alternatives% 
         [parent container]
         [choices-thunk (lambda () (list "Run in browser"
                                         "Build smartphone package"))]))
    #;(define ch (new choice% 
                  [label ""]
                  [choices (list "Run in browser"
                                 "Build smartphone package")]
                  [style '(horizontal-label)]
                  [parent container]))
  container)


(define (test)
  (define f (new frame% [label "test frame"]))
  (whalesong-tool-ui f
                     #:on-browser 
                     (lambda () 
                       (printf "on-browser\n"))
                     
                     #:on-build-package 
                     (lambda ()
                       (printf "on-build-package\n")))
  (send f show #t))