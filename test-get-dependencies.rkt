#lang racket
(require "get-dependencies.rkt"
         "get-module-bytecode.rkt"
         "parse-bytecode-5.1.1.rkt")
         
(define e
  (parse-bytecode (build-path "get-dependencies.rkt")))

(get-dependencies e)




 (get-dependencies (parse-bytecode (build-path "/home/dyoo/local/racket-5.1.1/lib/racket/collects/scheme/base.rkt")))