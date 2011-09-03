#!/usr/bin/env racket
#lang racket/base

(require "private/command.rkt"
         "parameters.rkt"
         "whalesong-helpers.rkt")

;; Command line for running Whalesong.


;; TODO: we may want to adopt this as a raco command, as described in:
;;
;;     http://docs.racket-lang.org/raco/command.html


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
   #:program "whalesong"
   #:argv (current-command-line-arguments)
   "The Whalesong command-line tool for compiling Racket to JavaScript"
   ["build-xhtml" "build a standalone xhtml package" 
            "Builds a Racket program and its required dependencies into a .xhtml file."
            #:once-each
            [("-v" "--verbose")
             ("Display verbose messages.")
             (current-verbose? #t)]
            [("--compress-javascript")
             ("Compress JavaScript with Google Closure (requires Java)")
             (current-compress-javascript? #t)]
            [("--dest-dir")
             dest-dir
             ("Set destination directory (default: current-directory)")
             (current-output-dir dest-dir)]
            #:args (path)
            (build-standalone-xhtml path)]
   ["build" "build a standalone html and javascript package" 
            "Builds a Racket program and its required dependencies into a .html and .js file."
            #:once-each
            [("-v" "--verbose")
             ("Display verbose messages.")
             (current-verbose? #t)]
            [("--compress-javascript")
             ("Compress JavaScript with Google Closure (requires Java)")
             (current-compress-javascript? #t)]
            [("--dest-dir")
             dest-dir
             ("Set destination directory (default: current-directory)")
             (current-output-dir dest-dir)]
            #:args (path)
            (build-html-and-javascript path)]
   
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


(at-toplevel)
