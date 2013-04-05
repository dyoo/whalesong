#lang racket

(require racket/sandbox
         racket/runtime-path
         setup/dirs
         racket/cmdline
         (for-syntax racket/base))


(define current-port (make-parameter 8080))
(void (command-line
       #:once-each 
       [("-p" "--port") p "Port (default 8080)" 
                        (current-port (string->number p))]))



(define-runtime-path server-path (build-path "server.rkt"))

(define (my-network-guard name str port role)
  (printf "I see: ~s ~s ~s ~s\n" name str port role)
  #t)


(define eval
  (parameterize ([sandbox-memory-limit 256]
                 [sandbox-output (current-output-port)]
                 [sandbox-network-guard my-network-guard])
    (printf "memory limit: ~s mb\n" (sandbox-memory-limit))
    (make-module-evaluator server-path
                           #:allow-read (list (build-path "/")))))
(printf "starting server thread\n")
(define server-thread (eval `(start-server #:port ,(current-port))))
(printf "thread started\n")
  
(sync server-thread)