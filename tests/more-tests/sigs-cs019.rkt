#lang planet dyoo/whalesong/cs019


(define: x : Number$ 3)
;(define: y : string? 4)
(check-expect x 3)


;; The error should focus around the signature "Number$" here.
#;(check-violation-highlights 
 (local [(define: x : Number$ "three")]
   'huh?)
 (list "Number$")
 (list 22))


(define: gl : (Listof: Any$) (list 1 "two" true))
(check-expect gl (list 1 "two" true))

(define: (g [x : Number$]) -> String$ "x")
(check-expect (g 10) "x")
#;(check-error (g "x"))
#;(check-violation-highlights
 (g "x")
 (list "Number$")
 (list 17))


(define: (g2 [x : Number$]) -> String$ 'not-a-string)
#;(check-error (g2 "10"))
#;(check-error (g2 10))
#;(check-violation-highlights
 (g2 "10")
 (list "Number$")
 (list 18))
#;(check-violation-highlights
 (g2 10)
 (list "String$")
 (list 31))


(define: (unchk [x : Any$]) -> Number$ (add1 x))
(check-expect (unchk 10) 11)
#;(check-error (unchk "x"))

(define: (f [x : Number$] [y : Number$]) -> Number$
  (+ x y))
(check-expect (f 10 12) 22)

(define: c1 : Char$ #\c)
(check-expect c1 #\c)


#;(define: key1 : Key$ "up")
#;(check-expect key1 "up")

(define: img1 : Image$ (circle 10 "solid" "red"))
(define: img2 : Image$ (overlay img1 img1))
(check-expect img1 (circle 10 "solid" "red"))
; (check-expect img1 img2) fails, expectedly, even though they are visually the same

(define: pos1 : Posn$ (make-posn 5 10))
(define: pos2 : Posn$ (make-posn 1 2))
(check-expect (posn-x pos1) 5)
(check-expect (posn-y pos2) 2)

(define-struct: swf ([f : (Number$ -> Number$)]))
(define a-swf (make-swf add1))
(check-expect ((swf-f a-swf) 10) 11)
#;(check-error (set-swf-f! a-swf 3))
#;(check-violation-highlights 


 (set-swf-f! a-swf 3)

 ;; What should be highlighted is the entire signature
 (list "(Number$ -> Number$)")
 (list 26))


(define: n*fn->n : (Number$ (Number$ -> Number$) -> Number$)
  (lambda (n1 fn) (fn n1)))
(check-expect (n*fn->n 5 add1) 6)
(check-expect (n*fn->n 10 sub1) 9)

(define broken-swf (make-swf add1))
(set-swf-f! broken-swf number->string) ;; first-order check succeeds
#;(check-error ((swf-f broken-swf) 3))  ;; contract violation
#;(check-violation-highlights
 ((swf-f broken-swf) 3)
 (list "Number$")
 (list 38)) ;; should be pointing to the Number$ in swf-f's range.

(check-expect (let ([a (make-swf add1)])
                (list ((swf-f a) 10)
                      (begin (set-swf-f! a sub1)
                             ((swf-f a) 10))))
              (list 11 9))

(define Tree$ (or: mt$ nd$))
(define-struct mt ())
(define mt$ (Sig: mt?))
(define-struct: nd ([v : Number$] [l : Tree$] [r : Tree$]))



#;(check-error (set-nd-v! (make-nd 0 (make-mt) (make-mt)) "x"))
#;(check-violation-highlights
 (set-nd-v! (make-nd 0 (make-mt) (make-mt)) "x")
 (list "Number$")
 (list 25))
#;(check-error (set-nd-l! (make-nd 0 (make-mt) (make-mt)) "x"))
#;(check-violation-highlights
 (set-nd-l! (make-nd 0 (make-mt) (make-mt)) "x")
 (list "(or: mt$ nd$)")
 (list 14))
(check-expect (let ([n (make-nd 0 (make-mt) (make-mt))])
                (begin
                  (set-nd-v! n 5)
                  (set-nd-l! n (make-nd 1 (make-mt) (make-mt)))
                  n))
              (make-nd 5 (make-nd 1 (make-mt) (make-mt)) (make-mt)))

(define: (a (t : mt$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) 1]))
(check-expect (a (make-mt)) 0)
#;(check-error (a (make-nd 1 2 3)))
#;(check-violation-highlights
 (a (make-nd 1 2 3))
 (list "(or: mt$ nd$)")
 (list 14))
#;(check-error (a 3))
#;(check-violation-highlights
 (a 3)
 (list "(Sig: mt?)")
 (list 12))



(define: (tree-sum (t : Tree$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) (+ (nd-v t) (tree-sum (nd-l t)) (tree-sum (nd-r t)))]))
(check-expect (tree-sum (make-nd 10 
                                 (make-nd 5 (make-mt) (make-mt))
                                 (make-nd 2 (make-nd 1 (make-mt) (make-mt)) (make-mt))))
              18)
#;(check-error (tree-sum (make-nd 10 
                                (make-nd 5 (make-mt) (make-mt))
                                (make-nd 2 (make-nd 1 (make-mt) 10) (make-mt)))))
#;(check-violation-highlights
 (tree-sum (make-nd 10 
                    (make-nd 5 (make-mt) (make-mt))
                    (make-nd 2 (make-nd 1 (make-mt) 10) (make-mt))))
 (list "(or: mt$ nd$)")
 (list 14))



(define: (prime? [n : (Sig: (lambda (n) (and (positive? n) (integer? n))))]) 
  -> Boolean$
  (local ([define (check k)
            (if (>= k n)
                true
                (if (= (remainder n k) 0)
                    false
                    (check (add1 k))))])
    (check 2)))
(check-expect (prime? 10) false)
(check-expect (prime? 5) true)
#;(check-error (prime? -1))
#;(check-violation-highlights 
 (prime? -1) 
 (list "(Sig: (lambda (n) (and (positive? n) (integer? n))))")
 (list 22))
#;(check-error (prime? 1.5))
#;(check-violation-highlights
 (prime? 1.5)
 (list "(Sig: (lambda (n) (and (positive? n) (integer? n))))")
 (list 22))



(define BadSig$ (or: (Number$ -> Number$) Number$))
;(define: bs : BadSig 3)
;(define BadSig2 (not: (Number$ -> Number$)))

(define VerySpecialNumber$ (and: Number$ (Sig: positive?) (Sig: even?)))
(define: vsn : VerySpecialNumber$ 20)
(check-expect vsn 20)
;(define: vsn2 : VerySpecialNumber$ 19)
(define: nvsn : (not: VerySpecialNumber$) 19)
(check-expect nvsn 19)

(define-struct: p ([x : Number$] [y : Number$]))
(define: (h [p : p$]) -> Number$
  (p-x p))
(check-expect (h (make-p 1 2)) 1)
(check-expect (h (make-p 3 4)) 3)

(define n->n (Sig: (Number$ -> Number$)))
(define: a1 : n->n add1)
(check-expect (a1 5) 6)
#;(check-error (a1 "x"))
#;(check-violation-highlights (a1 "x")
                            (list "Number$")
                            (list 20))

(define: a2 : (Number$ -> Number$) add1)
(check-expect (a2 5) 6)
#;(check-error (a2 "x"))
#;(check-violation-highlights (a2 "x")
                            (list "Number$")
                            (list 15))

(define: s2n : (String$ -> Number$) string->number)
(check-expect (s2n "123") 123)
#;(check-error (s2n "xyz")) ;; produces false
#;(check-violation-highlights (s2n "xyz")
                            (list "Number$")
                            (list 27))



(define: (i [f : (Number$ -> Number$)]) -> Number$
  (f 5))
(check-expect (i add1) 6)
#;(check-error (i number->string))
#;(check-violation-highlights 
 (i number->string)
 (list "Number$")
 (list 29))
#;(check-error (i string->number))
;; Unfortunately, the error that comes in isn't a signature error;
;; it encounters string->number first.
; #;(check-violation-highlights (i string->number) (list "Number$"))

(define: (j [f : (String$ String$ String$ -> Number$)]) -> Number$
  (f "12" "34" "56"))
(check-expect (j (lambda (s1 s2 s3)
                   (string->number (string-append s1 s2 s3))))
              123456)
#;(check-error (j string-append))
#;(check-violation-highlights
 (j string-append)
 (list "Number$")
 (list 45))

(define: (j2 [f : (String$ String$ -> String$)] [g : (String$ -> String$)]) -> String$
  (g (f "abc" "def")))
(check-expect (j2 string-append (lambda (s) (substring s 0 2))) "ab")

(define: (d/dx [f : (Number$ -> Number$)]) -> (Number$ -> Number$)
  (local [(define: dx : Number$ 0.0001)]
    (lambda: ([x : Number$]) -> Number$
      (/ (- (f (+ x dx)) (f x))
         dx))))
#;(check-within ((d/dx (lambda: ([x : Number$]) -> Number$ (* x x))) 10) 19 21)
#;(check-error ((d/dx number->string) 10))
#;(check-violation-highlights
 ((d/dx number->string) 10)
 (list "Number$")
 (list 32))

#;(check-within
 ((lambda: ([ddx : ((Number$ -> Number$) -> (Number$ -> Number$))]) -> Number$
    ((ddx (lambda (x) (* x x))) 10))
  d/dx)
 19 21)

(check-expect (local ([define: x : Number$ 3]) x) 3)
#;(check-error (local ([define: x : String$ 3]) x))
#;(check-violation-highlights
 (local ([define: x : String$ 3]) x)
 (list "String$")
 (list 22))



(check-expect (local ([define: (f [x : Number$]) -> String$
                        (number->string x)])
                (f 10))
              "10")
(check-expect (local ([define: (f [p : (Number$ -> String$)]) -> String$
                        (p 10)])
                (f number->string))
              "10")
#;(check-error (local ([define: (f [x : Number$]) -> String$
                       (number->string x)])
               (f "10")))
#;(check-violation-highlights
 (local ([define: (f [x : Number$]) -> String$
           (number->string x)])
   (f "10"))
 (list "Number$")
 (list 26))

(check-expect (local ([define-struct: m ([v : Number$] [w : String$])])
                (m-v (make-m 5 "x")))
              5)
#;(check-error (local ([define-struct: m ([v : Number$] [w : String$])])
               (m-v (make-m "x" 5))))
#;(check-violation-highlights
 (local ([define-struct: m ([v : Number$] [w : String$])])
   (m-v (make-m "x" 5)))
 (list "Number$")
 (list 33))
#;(check-error (local ([define-struct: m ([v : Number$] [w : String$])])
               (m-v (make-m 4 5))))
#;(check-violation-highlights
 (local ([define-struct: m ([v : Number$] [w : String$])])
   (m-v (make-m 4 5)))
 (list "String$")
 (list 47))




(define: l : (Listof: Number$) (list 1 2 3))
(check-expect l (list 1 2 3))
; (define: m : (Listof: Number$) (list 1 2 "X"))


(define: (n [l : (Listof: (Number$ -> Number$))]) -> (Listof: Number$)
  (map (lambda (f) (f 10)) l))
(check-expect (n (list add1 sub1)) (list 11 9))
#;(check-error (n (list add1 number->string)))
#;(check-violation-highlights
 (n (list add1 number->string))
 (list "Number$")
 (list 38))
#;(check-error (n (list add1 string->number)))


(define: vs : (Vectorof: String$)
  (vector "0" "1" "2"))

(check-expect vs (vector "0" "1" "2"))


(define: (cvts [ns : (Listof: Number$)]) -> (Listof: String$)
  (local [(define: cv : (Vectorof: ((Listof: String$) -> (Listof: String$)))
            (vector (lambda (cs) (cons "0" cs))
                    (lambda (cs) (cons "1" cs))))
          (define (iter ns)
            (if (empty? ns)
                ns
                ((vector-ref cv (first ns))
                 (iter (rest ns)))))]
    (iter ns)))
(check-expect (cvts empty) empty)
(check-expect (cvts (list 0 1 0 0)) (list "0" "1" "0" "0"))




(define S (or: 2 3))
#;(check-error (local [(define: v : S 4)]
               v))
#;(check-violation-highlights
 (local [(define: v : S 4)]
   v)
 (list "2")
 (list 15))
