#!/usr/bin/env racket
#lang racket/base

(require racket/list
         racket/string
         racket/match
         racket/file
         "make/make-structs.rkt"
         "js-assembler/package.rkt"
         "resource/structs.rkt"
         "private/command.rkt"
         "logger.rkt"
         "parameters.rkt"
         raco/command-name)


;; Usage:
;;
;; * Build standalone .xhtml application.
;;
;;     $ whalesong build main-module-name.rkt
;;
;;
;; * Print out the runtime library to standard output.
;;
;;     $ whalesong get-runtime
;;
;;
;; * Print out the JavaScript for the program.
;;
;;     $ whalesong get-javascript main-module-name.rkt



(define current-verbose? (make-parameter #f))
(define current-resource-dir (make-parameter
                              (build-path (current-directory) "res")))
(define current-write-resources? (make-parameter #t))


(define (at-toplevel)
  (svn-style-command-line
   #:program "whalesong"  ; (short-program+command-name)
   #:argv (current-command-line-arguments)
   "The Whalesong command-line tool for compiling Racket to JavaScript"
   ["build" "build a standalone xhtml package" 
            "Builds a Racket program and its required dependencies into a standalone .xhtml file."
            #:once-each
            [("-v" "--verbose")
             ("Display verbose messages.")
             (current-verbose? #t)]
            [("--compress-javascript")
             ("Compress JavaScript with Google Closure (requires Java)")
             (current-compress-javascript? #t)]
            
            #:args (path)
            (do-the-build path)]
   ["get-runtime" "print the runtime library to standard output"
                  "Prints the runtime JavaScript library that's used by Whalesong programs."
                  #:once-each
                  [("-v" "--verbose")
                   ("Display verbose messages.")
                   (current-verbose? #t)]
                  [("--compress-javascript")
                   ("Compress JavaScript with Google Closure (requires Java)")
                   (current-compress-javascript? #t)]

                  #:args ()
                  (print-the-runtime)]
   ["get-javascript" "Gets just the JavaScript code and prints it to standard output"
                     "Builds a racket program into JavaScript.  The outputted file depends on the runtime."
                     #:once-each
                     [("-v" "--verbose")
                      ("Display verbose messages.")
                      (current-verbose? #t)]

                     [("--compress-javascript")
                      ("Compress JavaScript with Google Closure (requires Java)")
                      (current-compress-javascript? #t)]

            
                     #:args (file)
                     (get-javascript-code file)]))
   
   


(define (turn-on-logger!)
  (void (thread (lambda ()
                  (let ([receiver
                         (make-log-receiver whalesong-logger
                                            (if (current-verbose?)
                                                'debug
                                                'info))])
                    (let loop ()
                      (let ([msg (sync receiver)])
                        (match msg
                          [(vector level msg data)
                           (fprintf (current-error-port)"~a: ~a\n" level msg)
                           (flush-output (current-error-port))]))
                      (loop)))))))
                    



   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (do-the-build f)
  (turn-on-logger!)
  (let-values ([(base filename dir?)
                (split-path f)])
    (let ([output-filename
           (build-path
            (regexp-replace #rx"[.](rkt|ss)$"
                            (path->string filename)
                            ".xhtml"))])
      (parameterize ([current-on-resource
                      (lambda (r)
                        (make-directory* (current-resource-dir))
                        (log-info (format "Writing resource ~s" (resource-path r)))
                        (copy-file (resource-path r) 
                                   (build-path (current-resource-dir)
                                               (resource-key r))))])
        (call-with-output-file* output-filename
                                (lambda (op)
                                  (package-standalone-xhtml
                                   (make-ModuleSource (build-path f))
                                   op))
                                #:exists 'replace)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-the-runtime)
  (turn-on-logger!)
  (display (get-runtime) (current-output-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-javascript-code filename)
  (turn-on-logger!)
  (display (get-standalone-code
            (make-ModuleSource (build-path filename)))
           (current-output-port)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(at-toplevel)
