#lang racket/base

(require racket/list
         racket/match
         racket/string
         "package.rkt"
         "sets.rkt")

;; Usage:
;;
;;
;; * Get complete list of dependency modules up to kernel
;;
;;     $ whalesong deps 
;;
;;
;; * Compile JavaScript object files (.jso)
;;
;;     $ whalesong compile [file.rkt] ...
;;
;;
;; * Build standalone application
;;
;;     $ whalesong build main-module-name.rkt


(define commands `((build 
                    ,(lambda (args)
                      (do-the-build args)))))

;; listof string
(define command-names (map (lambda (x) (symbol->string (car x)))
                           commands))


(define (print-expected-command)
  (printf "Expected one of the following: [~a].\n"
          (string-join command-names ", ")))

(define (at-toplevel)
  (define args (vector->list (current-command-line-arguments)))
  (cond [(empty? args)
         (print-expected-command)]
        [else
         (cond
          [(assoc (string->symbol (first args))
                  commands)
           =>
           (lambda (p)
             ((cadr p) (rest args)))]
          [else
           (printf "Unknown command ~s.\n" (first args))
           (print-expected-command)])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (do-the-build filenames)
  (let ([seen-module-names (new-set)])
    (let loop ([queue filenames])
      (void))))




(at-toplevel)
