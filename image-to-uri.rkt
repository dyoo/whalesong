#lang racket/base

(require file/convertible
         net/base64
         racket/contract
         2htdp/image)


(provide/contract [image->uri
                   (image? . -> . string?)])


;; This code is ripped out of the tracer library written by
;; Will Zimrin and Jeanette Miranda.

;; Translates image values to embeddable uris.  See:
;; http://en.wikipedia.org/wiki/Data_URI_scheme

;; returns the data-uri encoding of an image.
(define (image->uri img)
  (define base64-bytes (base64-encode (convert img 'png-bytes)))
  (string-append "data:image/png;charset=utf-8;base64,"
                 (bytes->string/utf-8 base64-bytes)))
