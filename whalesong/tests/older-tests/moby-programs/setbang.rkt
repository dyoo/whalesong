#lang s-exp "../../lang/wescheme.ss"


(printf "setbang.rkt\n")

(define some-value 16)
(check-expect (set! some-value 42)
              (void))


(define song '())

(define (bottles-of-beer)
  (let ([x 100])
    (define (loop)
      (if (< x 1)
	  (void)
	  (begin
	    (set! x (sub1 x))
	    (set! song (cons (format "~a bottles of beer on the wall\n" x)
			     song))
	    (loop))))
    (loop)))

(bottles-of-beer)


(check-expect (reverse song)
	      (list "99 bottles of beer on the wall\n"
		    "98 bottles of beer on the wall\n"
		    "97 bottles of beer on the wall\n"
		    "96 bottles of beer on the wall\n"
		    "95 bottles of beer on the wall\n"
		    "94 bottles of beer on the wall\n"
		    "93 bottles of beer on the wall\n"
		    "92 bottles of beer on the wall\n"
		    "91 bottles of beer on the wall\n"
		    "90 bottles of beer on the wall\n"
		    "89 bottles of beer on the wall\n"
		    "88 bottles of beer on the wall\n"
		    "87 bottles of beer on the wall\n"
		    "86 bottles of beer on the wall\n"
		    "85 bottles of beer on the wall\n"
		    "84 bottles of beer on the wall\n"
		    "83 bottles of beer on the wall\n"
		    "82 bottles of beer on the wall\n"
		    "81 bottles of beer on the wall\n"
		    "80 bottles of beer on the wall\n"
		    "79 bottles of beer on the wall\n"
		    "78 bottles of beer on the wall\n"
		    "77 bottles of beer on the wall\n"
		    "76 bottles of beer on the wall\n"
		    "75 bottles of beer on the wall\n"
		    "74 bottles of beer on the wall\n"
		    "73 bottles of beer on the wall\n"
		    "72 bottles of beer on the wall\n"
		    "71 bottles of beer on the wall\n"
		    "70 bottles of beer on the wall\n"
		    "69 bottles of beer on the wall\n"
		    "68 bottles of beer on the wall\n"
		    "67 bottles of beer on the wall\n"
		    "66 bottles of beer on the wall\n"
		    "65 bottles of beer on the wall\n"
		    "64 bottles of beer on the wall\n"
		    "63 bottles of beer on the wall\n"
		    "62 bottles of beer on the wall\n"
		    "61 bottles of beer on the wall\n"
		    "60 bottles of beer on the wall\n"
		    "59 bottles of beer on the wall\n"
		    "58 bottles of beer on the wall\n"
		    "57 bottles of beer on the wall\n"
		    "56 bottles of beer on the wall\n"
		    "55 bottles of beer on the wall\n"
		    "54 bottles of beer on the wall\n"
		    "53 bottles of beer on the wall\n"
		    "52 bottles of beer on the wall\n"
		    "51 bottles of beer on the wall\n"
		    "50 bottles of beer on the wall\n"
		    "49 bottles of beer on the wall\n"
		    "48 bottles of beer on the wall\n"
		    "47 bottles of beer on the wall\n"
		    "46 bottles of beer on the wall\n"
		    "45 bottles of beer on the wall\n"
		    "44 bottles of beer on the wall\n"
		    "43 bottles of beer on the wall\n"
		    "42 bottles of beer on the wall\n"
		    "41 bottles of beer on the wall\n"
		    "40 bottles of beer on the wall\n"
		    "39 bottles of beer on the wall\n"
		    "38 bottles of beer on the wall\n"
		    "37 bottles of beer on the wall\n"
		    "36 bottles of beer on the wall\n"
		    "35 bottles of beer on the wall\n"
		    "34 bottles of beer on the wall\n"
		    "33 bottles of beer on the wall\n"
		    "32 bottles of beer on the wall\n"
		    "31 bottles of beer on the wall\n"
		    "30 bottles of beer on the wall\n"
		    "29 bottles of beer on the wall\n"
		    "28 bottles of beer on the wall\n"
		    "27 bottles of beer on the wall\n"
		    "26 bottles of beer on the wall\n"
		    "25 bottles of beer on the wall\n"
		    "24 bottles of beer on the wall\n"
		    "23 bottles of beer on the wall\n"
		    "22 bottles of beer on the wall\n"
		    "21 bottles of beer on the wall\n"
		    "20 bottles of beer on the wall\n"
		    "19 bottles of beer on the wall\n"
		    "18 bottles of beer on the wall\n"
		    "17 bottles of beer on the wall\n"
		    "16 bottles of beer on the wall\n"
		    "15 bottles of beer on the wall\n"
		    "14 bottles of beer on the wall\n"
		    "13 bottles of beer on the wall\n"
		    "12 bottles of beer on the wall\n"
		    "11 bottles of beer on the wall\n"
		    "10 bottles of beer on the wall\n"
		    "9 bottles of beer on the wall\n"
		    "8 bottles of beer on the wall\n"
		    "7 bottles of beer on the wall\n"
		    "6 bottles of beer on the wall\n"
		    "5 bottles of beer on the wall\n"
		    "4 bottles of beer on the wall\n"
		    "3 bottles of beer on the wall\n"
		    "2 bottles of beer on the wall\n"
		    "1 bottles of beer on the wall\n"
		    "0 bottles of beer on the wall\n"))