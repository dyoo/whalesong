#lang racket/base

(require (planet ryanc/db)
	 "../version.rkt")


(when (not (file-exists? "whalesong-cache.sqlite3"))
   (define conn 
     (sqlite3-connect #:database "whalesong-cache.sqlite3"
		      #:mode 'create))
   (disconnect conn))
 

(define conn 
  (sqlite3-connect #:database "whalesong-cache.sqlite3"))


(define (cached? path)
  #f)

(define (save-in-cache! path)
  (void))