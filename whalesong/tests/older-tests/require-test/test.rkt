#lang racket/base

;; check to see that multiple invokations don't duplicate code generation.

(require "../../private/compile-moby-module.rkt"
         "../../private/module-record.rkt"
         racket/runtime-path)

(define-runtime-path m.rkt 
  "m.rkt"
  #;"/home/dyoo/Downloads/tmp/Package/tourguide.rkt")

(define (check-module-names-unique! module-records)
  (let ([names (map module-record-name module-records)])
    (unless (unique? names)
      (error 'check-module-names-unique!
             "modules with non-unique names: ~s" names))))


(define (unique? names)
  (let ([ht (make-hash)])
    (let/ec return
      (for ([n names])
        (cond [(hash-ref ht n #f)
               (return #f)]
              [else
               (hash-set! ht n #t)])
        (return #t)))))
             
        
  


(define (test)
  (define modules 
    (compile-moby-modules m.rkt))
  (check-module-names-unique! modules))


(test)