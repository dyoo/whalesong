#lang racket/base

(require (planet ryanc/db)
         (prefix-in whalesong: "../version.rkt")
         racket/file
         racket/path
         file/md5)

(define cache-directory-path
  (build-path (find-system-path 'pref-dir)
              "whalesong"))



;; create-cache-directory!: -> void
(define (create-cache-directory!)
  (unless (directory-exists? cache-directory-path)
    (make-directory* cache-directory-path)))
  

;; clear-cache-files!: -> void
;; Remove all the cache files.
(define (clear-cache-files!)
  (for ([file (directory-list cache-directory-path)])
    (when (file-exists? (build-path cache-directory-path file))
      (with-handlers ([exn:fail? void])
        (delete-file (build-path cache-directory-path file))))))
  
  
(define (ensure-cache-db-structure!)
  (when (not (file-exists? whalesong-cache.sqlite3))
    ;; Clear existing cache files: they're obsolete.
    (clear-cache-files!)
    (define conn 
      (sqlite3-connect #:database whalesong-cache.sqlite3
                       #:mode 'create))
    (query-exec conn
                (string-append
                 "create table cache(path string not null primary key, "
                 " md5sum string not null, "
                 "data blob not null);"))
    (query-exec conn
                "CREATE INDEX cache_md5sum_idx ON cache (md5sum);")
    (disconnect conn)))



(define whalesong-cache.sqlite3 
  (build-path cache-directory-path 
              (format "whalesong-cache-~a.sqlite"
                      whalesong:version)))


(create-cache-directory!)
(ensure-cache-db-structure!)

(define conn 
  (sqlite3-connect #:database whalesong-cache.sqlite3))


(define lookup-cache-stmt 
  (prepare conn (string-append "select path, md5sum, data "
                               "from cache "
                               "where path=? and md5sum=?")))
(define delete-cache-stmt 
  (prepare conn (string-append "delete from cache "
                               "where path=?")))
(define insert-cache-stmt 
  (prepare conn (string-append "insert into cache(path, md5sum, data)"
                               " values (?, ?, ?);")))


;; cached?: path -> (U false (vector string bytes (U string bytes)))
;; Returns a true value, (vector path md5-signature data), if we can
;; find an appropriate entry in the cache, and false otherwise.
(define (cached? path)
  (cond
    [(file-exists? path)
     (query-maybe-row conn 
                      lookup-cache-stmt 
                      (path->string path)
                      (call-with-input-file* path md5))]
    [else
     #f]))



;; save-in-cache!: path (U string bytes) -> void
;; Saves a record.
(define (save-in-cache! path data)
  (cond
    [(file-exists? path)
     (define signature (call-with-input-file* path md5))
     ;; Make sure there's a unique row/column by deleting
     ;; any row with the same key.
     (query-exec conn delete-cache-stmt (path->string path))
     (query-exec conn insert-cache-stmt
                 (path->string path)
                 signature
                 data)]
    [else
     (error 'save-in-cache! "File ~e does not exist" path)]))