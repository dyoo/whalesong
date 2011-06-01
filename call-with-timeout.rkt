#lang racket/base

(provide (struct-out exn:fail:timeout)
         call-with-timeout)


(define-struct (exn:fail:timeout exn:fail) (msecs))


(define-struct good-value (v))
(define-struct bad-value (exn))

;; call-with-timeout: (-> any) number -> any
;; Calls a thunk, with a given timeout.
(define (call-with-timeout thunk timeout)
  (let ([ch (make-channel)]
        [alarm-e
         (alarm-evt (+ (current-inexact-milliseconds)
                       timeout))])
    (let* ([cust (make-custodian)]
           [th (parameterize ([current-custodian cust])
                 (thread (lambda ()
                        (channel-put ch
                                     (with-handlers ([void
                                                      (lambda (e)
                                                        (make-bad-value e))])
                                       (make-good-value (thunk)))))))])
      (let ([result (sync ch
                          (handle-evt alarm-e
                                      (lambda (false-value)
                                        (begin0
                                            (make-bad-value
                                             (make-exn:fail:timeout
                                              "timeout"
                                              (current-continuation-marks)
                                              timeout))
                                          (custodian-shutdown-all cust)
                                          (kill-thread th)))))])
        (cond
         [(good-value? result)
          (good-value-v result)]
         [(bad-value? result)
          (raise (bad-value-exn result))])))))
        
