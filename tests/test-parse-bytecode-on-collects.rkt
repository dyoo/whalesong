#lang racket/base

;; Tries to parse all the files in collects and sees how long it takes.
;;
;; TODO: figure out why it fails to get the module bytecode for
;; collects/tests/matrix-test.rkt.  I'm seeing the following:
;; read-syntax: cannot load snip-class reader

(require "../parse-bytecode.rkt"
         racket/path)

(define collects-dir 
  (normalize-path
   (let ([p (find-system-path 'collects-dir)])
     (cond
       [(relative-path? p)
        (find-executable-path (find-system-path 'exec-file)
                              (find-system-path 'collects-dir))]
       [else
        p]))))


(for ([path (in-directory collects-dir)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "parsing file: ~a... " path)
    (flush-output)
    (let ([start-time (current-inexact-milliseconds)])
      (void (parse-bytecode path))
      (let ([end-time (current-inexact-milliseconds)])
        (printf "~a msecs\n" (inexact->exact (floor (- end-time start-time))))))))