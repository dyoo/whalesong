#lang setup/infotab

(define name "Whalesong")
(define blurb '("A Racket to JavaScript compiler"))
(define release-notes '((p "The JavaScript FFI includes js-function->procedure and js-async-function->procedure to lift JavaScript functions to Racket.  New world event types can be introduced with make-world-event-handler.")))
(define version "1.17")
(define primary-file "make-launcher.rkt")
(define categories '(devtools))
(define repositories '("4.x"))
(define required-core-version "5.1.1")

;; I am disabling the automatic launchers: it's causing issues with
;; file permissions.  The program "make-launcher.rkt" will build a
;; whalesong launcher, so I need to revise the instructions to use it
;; instead.
;;
;; (define racket-launcher-libraries '("whalesong.rkt"))
;; (define racket-launcher-names '("whalesong"))
;;

(define homepage "http://hashcollision.org/whalesong")
(define scribblings '(("scribblings/manual.scrbl")))
(define compile-omit-paths '("tests"
                             "sandbox"
                             "examples"
                             "experiments"
                             "simulator"
                             "tmp"))
(define can-be-loaded-with 'all)
