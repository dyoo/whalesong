#lang racket

(require "browser-harness.rkt")

;; Each of the tests below do a string-compare of the standard output
;; content vs. a text file with the same name, but with the .rkt file
;; type replaced with .expected.

(test "more-tests/hello.rkt")
(test "more-tests/simple-functions.rkt")
(test "more-tests/sk-generator.rkt")
(test "more-tests/sk-generator-2.rkt")
(test "more-tests/simple-structs.rkt")
(test "more-tests/man-vs-boy.rkt")
(test "more-tests/colors.rkt")
(test "more-tests/images.rkt")
(test "more-tests/lists.rkt")
(test "more-tests/simple-apply.rkt")
(test "more-tests/hello-bf.rkt")
(test "more-tests/conform.rkt")
(test "more-tests/earley.rkt")
