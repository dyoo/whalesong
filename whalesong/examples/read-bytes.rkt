#lang planet dyoo/whalesong

(let loop ([b (read-byte)])
  (cond
   [(eof-object? b)
    (void)]
   [else
    (display (string (integer->char b)))
    (loop (read-byte))]))