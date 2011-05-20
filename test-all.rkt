#lang racket

(require "test-parse.rkt"
         "test-parse-bytecode.rkt"
         "test-simulator.rkt"
         "test-compiler.rkt"
         "test-compiler-2.rkt"
         "test-assemble.rkt"
         "test-browser-evaluate.rkt"
         "test-package.rkt"
         "test-conform-browser.rkt"
         "test-earley-browser.rkt"
         "test-get-dependencies.rkt")


;; This test takes a bit too much time.
#;"test-conform.rkt"
