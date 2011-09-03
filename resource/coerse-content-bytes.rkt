#lang racket/base

(provide coerse-content-bytes)
(require (planet neil/html-parsing)
         (planet neil/html-writing))

;; coerse-content-bytes: path bytes -> bytes
;; Given some bytes, we may need to do some data validation or cleanup.
;; In particular, when the file's HTML, we need to make sure we're
;; storing syntactically valid HTML.
(define (coerse-content-bytes a-path bytes)
  (cond
   [(regexp-match #rx"\\.html$"
                  (path->string a-path))
    (define op (open-output-bytes))
    (write-html (html->xexp (open-input-bytes bytes)) op)
    (get-output-bytes op)]
   [else
    bytes]))