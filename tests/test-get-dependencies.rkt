#lang racket
(require "../get-dependencies.rkt"
         "../get-module-bytecode.rkt"
         "../parser/parse-bytecode.rkt"
         "../compiler/lexical-structs.rkt"
         racket/path
         racket/runtime-path
         rackunit)

(printf "test-get-dependencies.rkt\n")


(define-runtime-path get-dependencies-path
  (build-path ".." "get-dependencies.rkt"))
(define-runtime-path get-module-bytecode-path
  (build-path ".." "get-module-bytecode.rkt"))



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
  (string<? (symbol->string (ModuleLocator-name x))
            (symbol->string (ModuleLocator-name y))))



(define e
  (parse-bytecode get-dependencies-path))

(void (get-dependencies e))

(void (get-dependencies 
       (parse-bytecode 
        (build-path collects-dir "scheme" "base.rkt"))))





;; This should have three dependencies: racket/base, racket/match, and get-module-bytecode.
(let ([ip (open-input-string 
           (format (string-append "(module foo racket/base (require racket/math "
                                  "(file ~s)) (exp 1))")
                   (path->string (normalize-path get-module-bytecode-path))))])

  (check-equal? (sort (get-dependencies (parse-bytecode 
                                         (open-input-bytes 
                                          (get-module-bytecode ip))))
                      module-name<)

                (sort
                 (list (make-ModuleLocator 'collects/racket/base.rkt
                                           (normalize-path (build-path collects-dir "racket" "base.rkt")))
                       (make-ModuleLocator 'collects/racket/math.rkt
                                           (normalize-path (build-path collects-dir "racket" "math.rkt")))
                       (make-ModuleLocator 'whalesong/get-module-bytecode.rkt
                                           (normalize-path get-module-bytecode-path)))
                 module-name<)))