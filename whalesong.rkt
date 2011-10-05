#!/usr/bin/env racket
#lang racket/base

(require racket/runtime-path
         racket/path)

(define-runtime-path whalesong.cmd "whalesong-cmd.rkt")
(dynamic-require (normalize-path whalesong.cmd) #f)