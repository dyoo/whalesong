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
                   (build-frame))])
  (void))


(define NO-FILE-SELECTED "No file selected")


(define (build-frame)
  (define source-path #f)
  
  (define frame (new frame% [label "Build a Whalesong package"]))
  (define file-button (new button%
                           [parent frame]
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
  (define source-path-message (new message% [parent frame]
                                   [label NO-FILE-SELECTED]
                                   [auto-resize #t]))
  (define dest-dir-message (new message% [parent frame]
                                   [label ""]
                                   [auto-resize #t]))
  
  
  
  (define build-button (new button%
                            [parent frame]
                            [label "Build!"]
                            [enabled #f]
                            [callback (lambda (button event)
                                        (do-the-build source-path))]))
  (define options-panel (new group-box-panel%
                             [parent frame]
                             [label "Options"]))
  (new check-box% 
       [parent options-panel]
       [label "Compress JavaScript?"]
       [value (current-compress-javascript?)]
       [callback (lambda (c e) (current-compress-javascript? (send c get-value)))])
       
  (send frame show #t)
  (void))


(define (do-the-build source-path)
  (define f (new frame% [label "Building..."]))
  (define t (new text% [auto-wrap #t]))
  (define c (new editor-canvas% [parent f] [editor t]))
  (send f show #t)
  (thread (lambda ()
            (parameterize ([current-report-port (open-output-text-editor t)])
              (build-html-and-javascript source-path)
              (fprintf (current-report-port) "Build complete.")))))




#;(main)
(build-frame)