#lang racket/base

(require racket/gui/base
         racket/class
         racket/path
         "parameters.rkt"
         "whalesong-helpers.rkt"
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
                   (build-dialog))])
  (void))


(define NO-FILE-SELECTED "No file selected")


(define (build-dialog)
  (define source-path #f)
  
  (define dialog (new dialog% [label "Build a Whalesong package"]))
  (define file-button (new button%
                           [parent dialog]
                           [label "Choose file to build"]
                           [callback (lambda (button event)
                                       (set! source-path (get-file))
                                      
                                       (cond
                                         [source-path
                                          (current-output-dir
                                           (build-path (path-only source-path)
                                                       (let-values ([(_1 name _2)
                                                                     (split-path source-path)])
                                                         (regexp-replace #px"\\.\\w+$" (path->string name) ""))))
                                          (send source-path-message set-label
                                                (gui-utils:quote-literal-label
                                                 (format "~s selected."
                                                         (path->string source-path))))
                                          (send dest-dir-message set-label
                                                (gui-utils:quote-literal-label
                                                 (format "Output will be written to directory ~s."
                                                         (path->string (current-output-dir)))))
                                          (send build-button enable #t)]
                                         [else
                                          (send source-path-message set-label
                                                NO-FILE-SELECTED)
                                          (send build-button enabled #f)]))]))
  (define source-path-message (new message% [parent dialog]
                                   [label NO-FILE-SELECTED]
                                   [auto-resize #t]))
  (define dest-dir-message (new message% [parent dialog]
                                   [label ""]
                                   [auto-resize #t]))
  
  
  
  (define build-button (new button%
                            [parent dialog]
                            [label "Build!"]
                            [enabled #f]
                            [callback (lambda (button event)
                                        (do-the-build source-path))]))
  (define options-panel (new group-box-panel%
                             [parent dialog]
                             [label "Options"]))
  (new check-box% 
       [parent options-panel]
       [label "Compress JavaScript?"]
       [value (current-compress-javascript?)]
       [callback (lambda (c e) (current-compress-javascript? (send c get-value)))])
       
  (send dialog show #t)
  (void))


(define (do-the-build source-path)
  (build-html-and-javascript source-path)
  (message-box "Whalesong" "Build complete."))






#;(main)
(build-dialog)