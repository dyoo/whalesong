#lang racket

(require "browser-harness.rkt")

;; Each of the tests below do a string-compare of the standard output
;; content vs. a text file with the same name, but with the .rkt file
;; type replaced with .expected.

(test "more-tests/simple.rkt")
(test "more-tests/booleans.rkt")
(test "more-tests/checking.rkt")
(test "more-tests/string-tests.rkt")
(test "more-tests/chars.rkt")
(test "more-tests/numbers.rkt")
(test "more-tests/hello.rkt")
(test "more-tests/sharing.rkt")
(test "more-tests/printing.rkt")
(test "more-tests/simple-functions.rkt")
(test "more-tests/map.rkt")
(test "more-tests/quasi.rkt")
(test "more-tests/sk-generator.rkt")
(test "more-tests/sk-generator-2.rkt")
(test "more-tests/simple-structs.rkt")
(test "more-tests/man-vs-boy.rkt")
(test "more-tests/colors.rkt")
(test "more-tests/images.rkt")
(test "more-tests/lists.rkt")
(test "more-tests/fringe.rkt")
(test "more-tests/simple-apply.rkt")
(test "more-tests/isolating-bug.rkt")
(test "more-tests/hello-bf.rkt")
(test "more-tests/conform.rkt")
(test "more-tests/earley.rkt")
(test "more-tests/view.rkt")
(test "more-tests/weird-cc.rkt")
(test "more-tests/hashes.rkt")
(test "more-tests/hash-code.rkt")

(test "more-tests/booleans-cs019.rkt")
(test "more-tests/checking-cs019.rkt")
(test "more-tests/sharing-cs019.rkt")
(test "more-tests/basics-cs019.rkt")
(test "more-tests/sigs-cs019.rkt")
(test "more-tests/lists-cs019.rkt")
