#lang racket/base

(provide record-javascript-implementation!
         has-javascript-implementation?
         lookup-javascript-implementation
         
         record-redirection!
         follow-redirection

         #;record-exported-name!
         
         collect-redirections-to

         record-module-require!
         lookup-module-requires
         )


(define-struct record (path impl))
(define records '())

(define-struct redirection (from to))
(define redirections '())



(define-struct module-require (key path))
(define module-requires '())


;; record-javascript-implementation!: path string -> void
(define (record-javascript-implementation! a-path an-impl)
  (set! records (cons (make-record a-path an-impl)
                      records)))

;; has-javascript-implementation?: path -> boolean
(define (has-javascript-implementation? a-path)
  (printf "has-javascript-implementation: ~s\n"
          records)
  (let loop ([lst records])
    (cond
      [(null? lst)
       #f]
      [(equal? a-path (record-path (car lst)))
       #t]
      [else
       (printf "not equal\n~s\n~s\n" a-path (record-path (car lst)))
       (loop (cdr lst))])))


;; find: path (listof record) -> record
(define (find path lst)
  (cond
    [(null? lst)
     (error 'find "Couldn't find ~s" path)]
    [(equal? path (record-path (car lst)))
     (car lst)]
    [else
     (find path (cdr lst))]))


;; lookup-javascript-implementation: path -> module-path
(define (lookup-javascript-implementation a-path)
  (record-impl (find a-path records)))


(define (record-redirection! from to)
  (set! redirections (cons (make-redirection from to) redirections)))


(define (follow-redirection a-path)
  (let loop ([redirections redirections])
    (cond
      [(null? redirections)
       #f]
      [(equal? (redirection-from (car redirections)) a-path)
       (redirection-to (car redirections))]
      [else
       (loop (cdr redirections))])))



(define (record-module-require! key path)
  (set! module-requires
        (cons (make-module-require key path)
              module-requires)))


(define (lookup-module-requires key)
  (let loop ([requires module-requires])
    (cond
     [(null? requires)
      '()]
     [(equal? (module-require-key (car requires))
              key)
      (cons (module-require-path (car requires))
            (loop (cdr requires)))]
     [else
      (loop (cdr requires))])))



#;(define (record-exported-name! a-path internal-name external-name)
  (printf "I need to remember to export ~s as ~s\n" internal-name external-name)
  (void))


;; collect-redirections-to: path -> (listof path)
(define (collect-redirections-to a-path)
  (let loop ([redirections redirections])
    (cond
      [(null? redirections)
       '()]
      [(equal? (redirection-to (car redirections)) a-path)
       (redirection-from (car redirections))]
      [else
       (loop (cdr redirections))])))
