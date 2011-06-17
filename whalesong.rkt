#!/usr/bin/env racket
#lang racket/base

(require racket/list
         racket/string
         "make/make-structs.rkt"
         "js-assembler/package.rkt"
         "private/command.rkt"
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




(define (at-toplevel)
  (svn-style-command-line
   #:program "whalesong" #;(short-program+command-name)
   #:argv (current-command-line-arguments)
   "The Whalesong command-line tool for compiling Racket to JavaScript"
   ["build" "build a standalone xhtml package" 
            "Builds a Racket program and its required dependencies into a standalone .xhtml file."
            #:args (path)
            (do-the-build path)]
   ["get-runtime" "print the runtime library to standard output"
                  "Prints the runtime JavaScript library that's used by Whalesong programs."
                  #:args ()
                  (print-the-runtime)]
   ["get-javascript" "Gets just the JavaScript code and prints it to standard output"
                     "Builds a racket program into JavaScript.  The outputted file depends on the runtime."
                     #:args (file)
                     (get-javascript-code file)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (do-the-build f)
  (let-values ([(base filename dir?)
                (split-path f)])
    (let ([output-filename
           (build-path
            (regexp-replace #rx"[.](rkt|ss)$" (path->string filename) ".xhtml"))])
      (call-with-output-file* output-filename
                              (lambda (op)
                                (package-standalone-xhtml
                                 (make-ModuleSource (build-path f))
                                 op))
                              #:exists 'replace))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-the-runtime)
  (write-runtime (current-output-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-javascript-code filename)
  (write-standalone-code (make-ModuleSource (build-path filename)) (current-output-port)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(at-toplevel)
