#lang racket/base
(require racket/match)

;; A small module to provide logging for Whalesong.


(provide whalesong-logger)

(define whalesong-logger (make-logger 'whalesong))


(define (log-debug message . args)
  (log-message whalesong-logger
               'debug
               (apply format message args)
               #f))


(define (log-warning message . args)
  (log-message whalesong-logger
               'warning
               (apply format message args)
               #f))

(define (log-error message . args)
  (log-message whalesong-logger
               'error
               (apply format message args)
               #f))



(define should-print-logs? #t)
(define (set-whalesong-log-printing! v)
  (set! should-print-logs? v))

(void (thread (lambda ()
                (let ([receiver
                       (make-log-receiver whalesong-logger 'debug)])
                  (let loop ()
                    (let ([msg (sync receiver)])
                      (when should-print-logs?
                        (match msg
                          [(vector level msg data)
                           (printf "~a: ~a\n" level msg)]))
                      (loop)))))))
                    



(provide whalesong-logger log-debug log-warning log-error
         set-whalesong-log-printing!)