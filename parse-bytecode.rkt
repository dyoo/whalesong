#lang racket/base
(require "version-case/version-case.rkt"
         (for-syntax racket/base))

(version-case
  [(version>= (version) "5.1.1")
   (begin      
     (require "parse-bytecode-5.1.1.rkt")
     (provide (all-from-out "parse-bytecode-5.1.1.rkt")))]
  [else
   (error 'parse-bytecode "Whalesong doesn't have a compatible parser for Racket ~a" (version))])