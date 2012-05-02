#lang racket/base

(require racket/gui/base
         racket/class
         framework/gui-utils)

;; A minimal GUI just so that people aren't forced to deal with the command line.
;; 


(define (main)
  (define frame (new frame% [label "Whalesong"]))
  (send frame show #t)
  
  (define command-panel (new horizontal-panel% 
                             [parent frame]
                             [alignment '(center center)]))
  (new button% 
       [parent command-panel]
       [label "Build a package"]
       [callback (lambda (button event)
                   (build-dialog))]))


(define NO-FILE-SELECTED "No file selected")


(define (build-dialog)
  (define dialog (new dialog% [label "Build a package"]))
  (define source-path #f)
  (define file-button (new button%
                           [parent dialog]
                           [label "Choose file to build"]
                           [callback (lambda (button event)
                                       (set! source-path (get-file))
                                       (cond
                                         [source-path
                                          (send source-path-message set-label
                                                (gui-utils:quote-literal-label
                                                 (format "~s selected" (path->string source-path))))
                                          (send build-button enabled #t)]
                                         [else
                                          (send source-path-message set-label
                                                (format NO-FILE-SELECTED source-path))
                                          (send build-button enabled #f)]))]))
  (define source-path-message (new message% [parent dialog]
                                   [label NO-FILE-SELECTED]
                                   [auto-resize #t]))
  
  (define build-button (new button%
                            [parent dialog]
                            [label "Build!"]
                            [enabled #f]
                            [callback (lambda (button event)
                                        (do-the-build! #:source-file source-path))]))
  (send dialog show #t))


(define (do-the-build! #:source-file source-file)
  (void))











(main)