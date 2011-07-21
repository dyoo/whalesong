#lang racket/base

(provide inject-javascript-inline
         inject-javascript-src
         inject-empty-span-with-id)

(require scribble/core
         scribble/html-properties
         scriblib/render-cond)


;; Adds JavaScript if we're rendering in HTML.
(define (inject-javascript-inline . body)
  (cond-element 
   [latex ""]
   [html (make-element (make-style #f (list (make-script-property "text/javascript"
                                                           body)))
                '())]
   [text ""]))


(define (inject-javascript-src src)
  (cond-element
   [latex ""]
   [html 
    (make-element
     (make-style #f
                 (list 
                  (make-alt-tag "script")
                  (make-attributes
                   `((type . "text/javascript")
                     (src  . ,src)))))
     '())]

   [text ""]))


(define (inject-empty-span-with-id id)
  (cond-element
   [latex ""]
   [html 
    (make-element
     (make-style #f
                 (list 
                  (make-alt-tag "span")
                  (make-attributes
                   `((id . , id)))))
     '())]

   [text ""]))







;;(define (google-analytics)
;;  (make-tag 