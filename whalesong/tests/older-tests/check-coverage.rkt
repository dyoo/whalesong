#lang racket/base

(require "../private/get-wescheme-primitive-names.rkt"
	 racket/runtime-path
         racket/path
         racket/list)

(provide print-coverage-report untouched-wescheme-primitives)


(define-runtime-path program-path "moby-programs")

(define (read-module f)
  (parameterize ([read-accept-reader #t])
    (let loop ([sexp 
                (rest (first (rest (rest (rest (read 
                                                (open-input-file f)))))))])
      sexp)))
   

(define moby-program-sources
  (let loop ([files (directory-list program-path)])
    (cond
      [(empty? files)
       '()]
      [(and (file-exists? (build-path program-path (first files)))
            (bytes=? (or (filename-extension (first files)) #"")
                     #"rkt"))
       (cons (read-module (build-path program-path (first files)))
             (loop (rest files)))]
      [else
       (loop (rest files))])))
                     
(define (sexp-symbols an-sexp)
  (cond
    [(symbol? an-sexp)
     (list an-sexp)]
    [(pair? an-sexp)
     (append (sexp-symbols (car an-sexp))
             (sexp-symbols (cdr an-sexp)))]
    [else
     '()]))


;; untouched-wescheme-primitives: (listof symbol)
(define (untouched-wescheme-primitives)
  (let ([ht (make-hash)])
    (for ([sym wescheme-primitive-names])
      (hash-set! ht sym #t))
    
    (for ([sym (sexp-symbols moby-program-sources)])
      (hash-remove! ht sym))

    ;; manually remove some
    (hash-remove! ht '#%module-begin)
    (hash-remove! ht '#%app)
    (hash-remove! ht '#%datum)
    (hash-remove! ht '#%top)
    (hash-remove! ht '#%top-interaction)
    (hash-remove! ht 'planet)
    
    
    
    (for/list ([key (in-hash-keys ht)])
      key)))


(define (print-coverage-report)
  (let ([los (untouched-wescheme-primitives)])
    (unless (empty? los)
      (printf "~a PRIMITIVES UNTOUCHED BY TESTS!!!\nList below:\n" (length los))
      (for ([sym (sort los (lambda (x y) (string<? (symbol->string x)
                                                   (symbol->string y))))])
        (printf "~a\n" sym))
      (printf "\n"))))


(print-coverage-report)