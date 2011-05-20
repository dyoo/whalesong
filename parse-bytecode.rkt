#lang racket/base
(require "version-case/version-case.rkt"
         (for-syntax racket/base))

(version-case
  [(version>= (version) "5.1.1")
   (begin      
     (require "parse-bytecode-5.1.1.rkt")
     (provide parse-bytecode))]
  [else
   (error 'parse-bytecode "Currently no compatible parser for Racket ~a" (version))])