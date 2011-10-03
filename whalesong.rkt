#!/usr/bin/env racket
#lang racket/base

(require "private/command.rkt"
         "parameters.rkt"
         "whalesong-helpers.rkt"
         profile profile/render-text
         racket/path
         (for-syntax racket/base))

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

(define as-standalone-html? (make-parameter #f))
(define with-profiling? (make-parameter #f))

(define-syntax (maybe-with-profiling stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (if (with-profiling?)
           (profile expr
                    #:threads #t
                    #:delay 0.0001
                    #:render (lambda (profile)
                               (render profile
                                       #:truncate-source 500)))

           expr))]))


(define (set-root-path! root-path)
  (unless (directory-exists? root-path)
    (printf "ERROR: root path ~a does not appear to exist.\n" root-path)
    (printf "Aborting compilation.\n")
    (exit))
  (current-root-path (normalize-path root-path)))



(define (at-toplevel)
  (svn-style-command-line
   #:program "whalesong"
   #:argv (current-command-line-arguments)
   "The Whalesong command-line tool for compiling Racket to JavaScript"
   ["build" "build a standalone html and javascript package" 
            "Builds a Racket program and its required dependencies into a .html and .js file."
            #:once-each
            [("-v" "--verbose")
             ("Display verbose messages.")
             (current-verbose? #t)]
            [("--debug-show-timings")
             ("Display debug messages about compilation time.")
             (current-timing-port (current-output-port))]
            [("--enable-profiling")
             ("Enable profiling to standard output")
             (with-profiling? #t)]
            [("--without-ie")
             ("Disable IE legacy support")
             (current-with-legacy-ie-support? #f)]
            [("--without-cache")
             ("Turn off the internal compilation cache")
             (current-with-cache? #f)]
            [("--compress-javascript")
             ("Compress JavaScript with Google Closure (requires Java)")
             (current-compress-javascript? #t)]
            [("--split-modules")
             ("Write one file per module")
             (current-one-module-per-file? #t)]
            [("--root-dir")
             root-path
             ("Set the root package path (default: current-directory)")
             (set-root-path! root-path)]
            [("--dest-dir")
             dest-dir
             ("Set destination directory (default: current-directory)")
             (current-output-dir dest-dir)]
            [("--as-standalone-xhtml")
             ("Write single standalone xhtml file")
             (as-standalone-html? #t)]
            #:args (path)

            (maybe-with-profiling
             (if (as-standalone-html?)
                 (build-standalone-xhtml path)
                 (build-html-and-javascript path)))]
   
   ["get-runtime" "print the runtime library to standard output"
                  "Prints the runtime JavaScript library that's used by Whalesong programs."
                  #:once-each
                  [("-v" "--verbose")
                   ("Display verbose messages.")
                   (current-verbose? #t)]
                  [("--debug-show-timings")
                   ("Display debug messages about compilation time.")
                   (current-timing-port (current-output-port))]
                  [("--enable-profiling")
                   ("Enable profiling to standard output")
                   (with-profiling? #t)]
                  [("--without-ie")
                   ("Disable IE legacy support")
                   (current-with-legacy-ie-support? #f)]
                  [("--without-cache")
                   ("Turn off the internal compilation cache")
                   (current-with-cache? #f)]
                  [("--root-dir")
                   root-path
                   ("Set the root package path (default: current-directory)")
                   (set-root-path! root-path)]
                  [("--compress-javascript")
                   ("Compress JavaScript with Google Closure (requires Java)")
                   (current-compress-javascript? #t)]
                  
                  #:args ()
                  (maybe-with-profiling
                   (print-the-runtime))]
   ["get-javascript" "Gets just the JavaScript code and prints it to standard output"
                     "Builds a racket program into JavaScript.  The outputted file depends on the runtime."
                     #:once-each
                     [("-v" "--verbose")
                      ("Display verbose messages.")
                      (current-verbose? #t)]
                     [("--debug-show-timings")
                      ("Display debug messages about compilation time.")
                      (current-timing-port (current-output-port))]
                     [("--enable-profiling")
                      ("Enable profiling to standard output")
                      (with-profiling? #t)]
                     [("--without-ie")
                      ("Disable IE legacy support")
                      (current-with-legacy-ie-support? #f)]
                     [("--without-cache")
                      ("Turn off the internal compilation cache")
                      (current-with-cache? #f)]
                     [("--root-dir")
                      root-path
                      ("Set the root package path (default: current-directory)")
                      (set-root-path! root-path)]
                     
                     [("--compress-javascript")
                      ("Compress JavaScript with Google Closure (requires Java)")
                      (current-compress-javascript? #t)]
                     
                     
                     #:args (file)
                     (maybe-with-profiling
                      (get-javascript-code file))]))


(at-toplevel)
