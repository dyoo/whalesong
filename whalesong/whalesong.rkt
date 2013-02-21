#!/usr/bin/env racket
#lang racket/base

(require racket/runtime-path
         racket/path
         planet/util)

;; We do things this way to ensure that we're using the latest
;; version of whalesong that's installed, and that the load-relative
;; path is in terms of the normalized paths, to avoid a very strange
;; low-level bug.
(define whalesong.cmd
  (resolve-planet-path '(planet dyoo/whalesong/whalesong-cmd)))

(dynamic-require (normalize-path whalesong.cmd) #f)