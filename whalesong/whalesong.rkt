#!/usr/bin/env racket
#lang racket/base

(require racket/runtime-path
         racket/path
         syntax/modresolve)

;; We do things this way to ensure that we're using the latest
;; version of whalesong that's installed, and that the load-relative
;; path is in terms of the normalized paths, to avoid a very strange
;; low-level bug.
(define whalesong.cmd
  (resolve-module-path 'whalesong/whalesong-cmd #f))

(dynamic-require (normalize-path whalesong.cmd) #f)
