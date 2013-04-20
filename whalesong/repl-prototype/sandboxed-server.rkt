#lang racket

(require racket/sandbox
         racket/runtime-path
         setup/dirs
         racket/cmdline
         (for-syntax racket/base))


(define current-port (make-parameter 8080))
(define current-memory-limit (make-parameter 256))
(void (command-line
       #:once-each 
       [("-p" "--port") p "Port (default 8080)" 
                        (current-port (string->number p))]
       [("--memory-limit") memlimit "Memory limit in MB (default 256)"
                           (current-memory-limit (string->number memlimit))]))



(define-runtime-path server-path (build-path "server.rkt"))

(define (my-network-guard name str port role)
  (printf "I see: ~s ~s ~s ~s\n" name str port role)
  #t)


(let loop ()
  (parameterize ([sandbox-memory-limit (current-memory-limit)]
                 [sandbox-eval-limits '(+inf.0 256)]
                 [sandbox-output (current-output-port)]
                 [sandbox-network-guard my-network-guard])
    (printf "memory limit: ~s mb\n" (sandbox-memory-limit))
    (define eval
      (make-module-evaluator server-path
                             #:allow-read (list (build-path "/"))))
    (printf "starting server thread\n")
    (eval 
     `(begin (define server-thread (start-server #:port ,(current-port)))
             (printf "thread started\n")
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (printf "server died prematurely?  ~s\n" 
                                        (exn-message exn)))])
               (sync server-thread))))
    (printf "restarting server\n")
    (loop)))
