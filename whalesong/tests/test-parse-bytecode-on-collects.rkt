#lang racket/base

;; Tries to parse all the files in collects and sees how long it takes.
;;
;; Files that take too long get categorized as timeouts.


(require "../parser/parse-bytecode.rkt"
         "../call-with-timeout.rkt"
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
                                   (set! failures
                                         (cons (list (path->string path)
                                                     (exn-message exn))
                                               failures))
                                   (printf "FAILED: ~a\n" (exn-message exn)))))
        (call-with-timeout (lambda ()
                             (void (parse-bytecode path)))
                           ;; timeout
                           1000)
        (let ([end-time (current-inexact-milliseconds)])
          (printf "~a msecs\n" (inexact->exact (floor (- end-time start-time)))))))))



(unless (empty? failures)
  (printf "Failed on:\n")
  (for ([f failures])
    (printf "~s\n" f)))