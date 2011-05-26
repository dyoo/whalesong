#lang racket/base

;; Tries to parse all the files in collects and sees how long it takes.
;;
;; TODO: figure out why it fails to get the module bytecode for
;; collects/tests/matrix-test.rkt.  I'm seeing the following:
;; read-syntax: cannot load snip-class reader

(require "../parser/parse-bytecode.rkt"
         racket/list
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

(define failures '())

(for ([path (in-directory collects-dir)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "parsing file: ~a... " path)
    (flush-output)
    (let ([start-time (current-inexact-milliseconds)])
      (with-handlers ((exn:fail? (lambda (exn)
                                   (set! failures (cons path failures))
                                   (printf "FAILED!  ~a" (exn-message exn)))))
        (void (parse-bytecode path))
        (let ([end-time (current-inexact-milliseconds)])
          (printf "~a msecs\n" (inexact->exact (floor (- end-time start-time)))))))))

(unless (empty? failures)
  (printf "Failed on: ~s" failures))