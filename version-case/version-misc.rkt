#lang racket/base
(require racket/string
         racket/list
         racket/contract
         (prefix-in 67: srfi/67)
         (prefix-in 1: srfi/1))

(provide/contract [version<= (string? string? . -> . boolean?)]
                  [version< (string? string? . -> . boolean?)]
                  [version= (string? string? . -> . boolean?)]
                  [version>= (string? string? . -> . boolean?)]
                  [version> (string? string? . -> . boolean?)])


;; The definitions of mz-version, string->version, and
;; version<= were adapted (copied and pasted) from PLaneT's
;; implementation in (planet/private/planet-shared.ss).

(define-struct mz-version (numbers) #:transparent)

;; string->version : string -> mz-version | #f
(define (string->version str)
  (cond
   ;; Old style numbering (with three digits in front)
   [(regexp-match #rx"^([0-9][0-9][0-9])([.0-9]*)$" str)
    =>
    (lambda (ver)
      (let* ([major (string->number (list-ref ver 1))]
             [after-major
              (map string->number
                   (rest (regexp-split "\\." (list-ref ver 2))))]
             [minor (if (>= (length after-major) 1)
                        (first after-major)
                        0)]
             [maintenances (my-drop after-major 1)])
        (make-mz-version (list*
                          (remainder (quotient major 100) 10)
                          (remainder (quotient major 10) 10)
                          (remainder major 10)
                          minor
                          maintenances))))]
   ;; New style numbering
   [(regexp-match #rx"^([.0-9]*)$" str)
    =>
    (lambda (ver)
      (let* ([numbers (regexp-split "\\." (list-ref ver 1))])
        (make-mz-version (map string->number numbers))))]
   [else #f]))


;; drop: (listof X) number -> (listof X)
;; A more permissive version of drop that returns the empty list
;; if we try to take off too many elements.
(define (my-drop a-list n)
  (1:drop a-list (min n (length a-list))))



;; version-cmp: mz-version mz-version -> (union -1 0 1)
;; Returns -1 if v1 < v2, 0 if v1 = v2, and 1 if v1 > v2.
(define (version-cmp v1 v2)
  (67:list-compare 67:integer-compare
                   (mz-version-numbers v1)
                   (mz-version-numbers v2)))


;; version<= : string string -> boolean
;; determines if a is the version string of an earlier
;; mzscheme release than b
;; [n.b. this relies on a guarantee from Matthew that
;; mzscheme version x1.y1 is older than version x2.y2 iff
;;  x1<x2 or x1=x2 and y1<y2]
(define (version<= a b)
  (let ([a (string->version a)]
        [b (string->version b)])
    (not (= (version-cmp a b)
            1))))

(define (version>= a b)
  (let ([a (string->version a)]
        [b (string->version b)])
    (not (= (version-cmp a b)
            -1))))

(define (version= a b)
  (let ([a (string->version a)]
        [b (string->version b)])
    (= (version-cmp a b)
       0)))

(define (version< a b)
  (let ([a (string->version a)]
        [b (string->version b)])
    (= (version-cmp a b)
       -1)))

(define (version> a b)
  (let ([a (string->version a)]
        [b (string->version b)])
    (= (version-cmp a b)
       1)))