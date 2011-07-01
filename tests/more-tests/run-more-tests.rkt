#lang racket

(require "../browser-harness.rkt")

;; Each of the tests below do a string-compare of the standard output
;; content vs. a text file with the same name, but with the .rkt file
;; type replaced with .expected.

(test "hello.rkt")
(test "sk-generator.rkt")
#;(test "simple-structs.rkt")