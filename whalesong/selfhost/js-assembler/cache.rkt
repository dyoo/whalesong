#lang whalesong
(define-struct cached-entry (real-path ;; path to a module. 
                             whalesong-version ;; string
                             md5   ;; md5 of the original source in real-path
                             bytes)
  #:transparent) ;; bytes

