#lang racket/base

(require (planet ryanc/db)
         (prefix-in whalesong: "../version.rkt")
         racket/file
         racket/path)

(define cache-directory-path
  (build-path (find-system-path 'pref-dir)
              "whalesong"))



;; create-cache-directory!: -> void
(define (create-cache-directory!)
  (unless (directory-exists? cache-directory-path)
    (make-directory* cache-directory-path)))
  
(create-cache-directory!)

(define whalesong-cache.sqlite3 
  (build-path cache-directory-path 
              (format "whalesong-cache-~a.sqlite"
                      whalesong:version)))


(when (not (file-exists? whalesong-cache.sqlite3))
  (define conn 
     (sqlite3-connect #:database whalesong-cache.sqlite3
		      #:mode 'create))
  (query-exec conn
              (string-append
               "create table cache(path string not null primary key, "
               " md5sum string not null, "
               "data blob not null);"))
  (disconnect conn))
 

(define conn 
  (sqlite3-connect #:database whalesong-cache.sqlite3))


(define (cached? path)
  #f)

(define (save-in-cache! path)
  (void))