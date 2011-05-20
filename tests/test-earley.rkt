#lang racket

(require "../simulator/simulator.rkt"
         "../simulator/simulator-structs.rkt"
         "test-helpers.rkt"
         racket/runtime-path)

(define-runtime-path earley-path (build-path "earley"))


  
;; run: machine -> (machine number)
;; Run the machine to completion.
(define (run m 
             #:debug? (debug? false)
             #:stack-limit (stack-limit false)
             #:control-limit (control-limit false))

  #;(for-each displayln (vector->list (machine-text m)))
  
  (let loop ([steps 0])
    (when debug?
      (when (can-step? m)
        (printf "pc=~s, |env|=~s, |control|=~s,  instruction=~s\n" 
                (machine-pc m)
                (length (machine-env m))
                (length (machine-control m))
                (current-instruction m))))
    (when stack-limit
      (when (> (machine-stack-size m) stack-limit)
        (error 'run "Stack overflow")))
    
    (when control-limit
      (when (> (machine-control-size m) control-limit)
        (error 'run "Control overflow")))

    (cond
      [(can-step? m)
       (step! m)
       (loop (add1 steps))]
      [else
       (values m steps)])))

;; Test out the compiler, using the simulator.
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ code exp options ...)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "Running... \n")
           (let*-values([(a-machine num-steps) 
                         (run (new-machine (run-compiler code) #t) options ...)]
                        [(actual) (machine-val a-machine)])
             (printf "ok. ~s steps.\n\n" num-steps)))))]))


(test (read (open-input-file (build-path earley-path "earley.sch")))
      (port->string (open-input-file (build-path earley-path "expected.txt"))))
