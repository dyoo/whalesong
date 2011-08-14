#lang racket/base

;; Implements a button with alternatives.

(require racket/gui/base
         racket/class)

(define (whalesong-tool-ui parent-widget
                           #:on-browser (on-browser
                                         (lambda ()
                                           (void)))
                           #:on-build-package (on-build-package 
                                               (lambda ()
                                                 (void))))
  (define container (new horizontal-pane%
                         [parent parent-widget]))
  (define b (new button% 
                 [label "Whalesong"]
                 [callback (lambda (b ce)
                             (define selection 
                               (send ch get-selection))
                             (cond
                               [(= selection 0)
                                (on-browser)]
                               [(= selection 1)
                                (on-build-package)]
                               [else
                                (void)]))]
                 [parent container]))
  (define ch (new choice% 
                  [label ""]
                  [choices (list "Run in browser"
                                 "Build smartphone package")]
                  [style '(horizontal-label)]
                  [parent container]))
  container)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define f (new frame% [label "test frame"]))
(whalesong-tool-ui f
                   #:on-browser 
                   (lambda () 
                     (printf "on-browser\n"))

                   #:on-build-package 
                   (lambda ()
                     (printf "on-build-package\n")))
(send f show #t)