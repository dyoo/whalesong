#lang racket
(require "get-dependencies.rkt"
         "get-module-bytecode.rkt"
         "parse-bytecode-5.1.1.rkt"
         "lexical-structs.rkt"
         racket/path
         racket/runtime-path
         rackunit)

(define-runtime-path this-path ".")

(define e
  (parse-bytecode (build-path this-path "get-dependencies.rkt")))
(void (get-dependencies e))

(void (get-dependencies 
       (parse-bytecode 
        (build-path 
         "/home/dyoo/local/racket-5.1.1/lib/racket/collects/scheme/base.rkt"))))



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




;; This should have three dependencies: racket/base, racket/match, and get-module-bytecode.
(check-equal? 
 (sort (get-dependencies (parse-bytecode 
                          (open-input-bytes 
                           (get-module-bytecode 
                            (open-input-string (format #<<EOF
(module foo racket/base 
  (require racket/math
           (file "~a/get-module-bytecode.rkt"))
  (exp 1))
EOF
                                                       (path->string (normalize-path this-path)))
                                               )))))
       module-name<)
 (sort
  (list (make-ModuleName 'collects/racket/base.rkt
                         (build-path collects-dir "racket" "base.rkt"))
        (make-ModuleName 'collects/racket/math.rkt
                         (build-path collects-dir "racket" "math.rkt"))
        (make-ModuleName 'whalesong/get-module-bytecode.rkt
                         (normalize-path (build-path this-path "get-module-bytecode.rkt"))))
  module-name<))