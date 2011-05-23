#lang racket
(require "../get-dependencies.rkt"
         "../get-module-bytecode.rkt"
         "../parse-bytecode.rkt"
         "../lexical-structs.rkt"
         racket/path
         racket/runtime-path
         rackunit)

(printf "test-get-dependencies.rkt\n")

(define-runtime-path compiler-path "..")


;(printf "This is the path: ~s\n"
;        (path->string (normalize-path compiler-path)))
;(flush-output)



(define collects-dir 
  (normalize-path
   (let ([p (find-system-path 'collects-dir)])
     (cond
       [(relative-path? p)
        (find-executable-path (find-system-path 'exec-file)
                              (find-system-path 'collects-dir))]
       [else
        p]))))



(define (module-name< x y)
  (string<? (symbol->string (ModuleName-name x))
            (symbol->string (ModuleName-name y))))



(define e
  (parse-bytecode (build-path compiler-path "get-dependencies.rkt")))

(void (get-dependencies e))

(void (get-dependencies 
       (parse-bytecode 
        (build-path collects-dir "scheme" "base.rkt"))))





;; This should have three dependencies: racket/base, racket/match, and get-module-bytecode.
(let ([ip (open-input-string 
           (format (string-append "(module foo racket/base (require racket/math "
                                  "(file \"~a/get-module-bytecode.rkt\")) (exp 1))")
                   (path->string (normalize-path compiler-path))))])

  (check-equal? (sort (get-dependencies (parse-bytecode 
                                         (open-input-bytes 
                                           (get-module-bytecode ip))))
                      module-name<)

                (sort
                 (list (make-ModuleName 'collects/racket/base.rkt
                                        (normalize-path (build-path collects-dir "racket" "base.rkt")))
                       (make-ModuleName 'collects/racket/math.rkt
                                        (normalize-path (build-path collects-dir "racket" "math.rkt")))
                       (make-ModuleName 'whalesong/get-module-bytecode.rkt
                                        (normalize-path (build-path compiler-path "get-module-bytecode.rkt"))))
                 module-name<)))