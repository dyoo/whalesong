#lang whalesong (require "selfhost-lang.rkt" whalesong/lang/for)

(provide string-replace) ; (string-replace k r s) replace all occurences of k in s with r

; string-index : string string [integer] -> integer
;   return the index of the first occurence of k in the string s
;   whose index is from or greater
(define (string-index k s [from 0])
  (define kn (string-length k))
  (define sn (string-length s))
  (and (<= (+ from kn) sn)
       (for/or ([i (in-range from (- sn kn -1))])
         (and (for/and ([j (in-range i (+ i kn))] [l (in-range kn)])
                (char=? (string-ref s j) (string-ref k l)))
              i))))

; a new string is returned where occurences of the string k (the key)
; are replaced with the string r (the replacement) in the string s.
(define (string-replace k r s)
  (define kn (string-length k))
  (define sn (string-length s))
  (let loop ([start 0] [from 0] [chunks '()])
    (define i (string-index k s from))
    (displayln (list 'loop start from chunks i))
    (cond
      [i    
       (define new-start (+ i kn))
       (loop new-start
             new-start
             (cons r (cons (substring s start i) chunks)))]
      [(empty? chunks) 
       (string-copy s)]
      [else
       (apply string-append
         (reverse (cons (if (<= start sn)
                            (substring s start)
                            "")
                        chunks)))])))

; Test must evaluate to #t
#;(and (= (string-index "bar" "foobarbazbar")
        (string-index "bar" "foobarbazbar" 1)
        (string-index "bar" "foobarbazbar" 2)
        (string-index "bar" "foobarbazbar" 3) 
        3)
     (= (string-index "bar" "foobarbazbar" 4) 9)
     (equal? (string-index "bar" "foobarbazbar" 10) #f))

; (string-replace "foo" "_" "abfoocdfoooo")


