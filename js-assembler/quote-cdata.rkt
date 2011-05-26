#lang typed/racket/base

;; quoting cdata for script tags.  This is used to help generate SCRIPT bodies in XHTML.
;; Note that this won't help too much in regular HTML5 documents.

(require racket/list)
(require/typed racket/base (regexp-split (Regexp String -> (Listof String))))

(provide quote-as-cdata get-cdata-chunks)


(: quote-as-cdata (String -> String))
(define (quote-as-cdata str)
  (let ([chunks (regexp-split #rx"\\]\\]>" str)])
    (apply string-append (map wrap (process chunks)))))


(: get-cdata-chunks (String -> (Listof String)))
(define (get-cdata-chunks s)
  (let ([chunks (regexp-split #rx"\\]\\]>" s)])
    (process chunks)))


(: process ((Listof String) -> (Listof String)))
(define (process lst)
  (cond
    [(empty? (rest lst))
     lst]
    [else
     (cons (string-append (first lst) "]]")
           (process (cons (string-append ">" (second lst))
                          (rest (rest lst)))))]))

(: wrap (String -> String))          
(define (wrap s)
  (string-append "<![CDATA[" s "]]>"))