#lang s-exp syntax/module-reader
#:language (lambda () 'whalesong/bf/language)
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
(require "../parser.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (parse-expr src in))



;; Extension: we'd like to cooperate with DrRacket and tell
;; it to use the default, textual lexer and color scheme when
;; editing bf programs.
;;
;; See: http://docs.racket-lang.org/guide/language-get-info.html
;; for more details, as well as the documentation in
;; syntax/module-reader.
(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer
                      'default-lexer)]
    [else
     (default-filter key default)]))

