#lang whalesong
(require whalesong/lang/for)

; Implements http://en.wikipedia.org/wiki/Base64

(provide base64-encode) ; string -> string

(define (bytes-ref bs i)
  (define c (string-ref bs i))
  (char->integer c))

(define (string->bytes s)
  (for/list ([c (in-string s)])
    (char->integer c)))

; 
(define ranges '(["AZ" 0]    ;  0 to 25
                 ["az" 26]   ; 16 to 51
                 ["09" 52]   ; 52 to 61
                 ["++" 62]   ; 62 
                 ["//" 63])) ; 63

; > (vector-ref base64-digit (char->integer #\A)) 
; 0
; > (vector-ref digit-base64 0) 
; 65 (which is #\A)

(define-values (base64-digit digit-base64)
  (let ([bd (make-vector 256 #f)] 
        [db (make-vector 64 #f)]) 
    (for ([r ranges] #:when #t
                     [i (in-range (bytes-ref (car r) 0) (add1 (bytes-ref (car r) 1)))]
                     [n (in-naturals (cadr r))])
      (vector-set! bd i n)
      (vector-set! db n i))
    (values bd db)))

(define (3bytes->24bit a b c)
  ; convert 3 bytes into  a single 24 bit number
  (+ (* a 65536) (* b 256) c))

(define (24bit->base64 n)
  ; convert a 24 bit number into base 64
  (define a  (remainder n      64))
  (define n1 (quotient  n      64))
  (define b  (remainder n1     64))
  (define n2 (quotient  n1     64))
  (define c  (remainder n2     64))
  (define d  (quotient  n2     64))
  (list d c b a))

(define =byte (bytes-ref "=" 0))

(define (base64-encode s)
  (define sn (string-length s))
  (define (encode s)
    (define n sn)
    (define ds
      (for/list ([i (in-range 0 n 3)])
        (define a (bytes-ref s i))
        (define b (bytes-ref s (+ i 1)))
        (define c (bytes-ref s (+ i 2)))
        (for/list ([digit (24bit->base64 (3bytes->24bit a b c))])
          (integer->char (vector-ref digit-base64 digit)))))
    (define padding  (case (remainder sn 3) [(0) 0]   [(1) 2]              [(2) 1]))
    (define padding= (case (remainder sn 3) [(0) '()] [(1) (list #\= #\=)] [(2) (list #\=)]))
    (define ds* (apply append ds))
    (list->string (reverse (append padding= (drop (reverse ds*) padding)))))

  (case (remainder sn 3)
    [(0) (encode s)]
    [(1) (encode (string-append s (string (integer->char 0) (integer->char 0))))]
    [(2) (encode (string-append s (string (integer->char 0))))]))

