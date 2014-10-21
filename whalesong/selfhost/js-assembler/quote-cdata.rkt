#lang whalesong (require "../selfhost-lang.rkt" "../selfhost-strings.rkt")
; #lang typed/racket/base

;; quoting cdata for script tags.  This is used to help generate SCRIPT bodies in XHTML.
;; Note that this won't help too much in regular HTML5 documents.

(provide quote-cdata)

; CDATA (character data) sequence begins with
;                <![CDATA[
; and ends with  ]]>

; A CDATA section can not contain ]]> 
; To encode, say, ]]> one would write <![CDATA[]]]]><![CDATA[>]]>
; I.e. replace all occurences of ]]> with ]]]]><![CDATA[>
; which stops and restarts the cdata sequence.

(: quote-cdata (String -> String))
#;(define (quote-cdata s)
  (string-append "<![CDATA["
                 (regexp-replace* #rx"]]>"             ; pattern
                                  s                    ; input
                                  "]]]]><![CDATA[>")   ; insert
                 "]]>"))

(define (quote-cdata s)
  (string-append "<![CDATA["
                 (string-replace "]]>"
                                 "]]]]><![CDATA[>"
                                 s)
                 "]]>"))
                                  

;; (: quote-cdata (String -> String))
;; (define (quote-cdata str)
;;   (let ([chunks (regexp-split #rx"\\]\\]>" str)])
;;     (apply string-append (map wrap (process chunks)))))


;; (: get-cdata-chunks (String -> (Listof String)))
;; (define (get-cdata-chunks s)
;;   (let ([chunks (regexp-split #rx"\\]\\]>" s)])
;;     (process chunks)))


;; (: process ((Listof String) -> (Listof String)))
;; (define (process lst)
;;   (cond
;;     [(empty? (rest lst))
;;      lst]
;;     [else
;;      (cons (string-append (first lst) "]]")
;;            (process (cons (string-append ">" (second lst))
;;                           (rest (rest lst)))))]))

;; (: wrap (String -> String))          
;; (define (wrap s)
;;   (string-append "<![CDATA[" s "]]>"))
