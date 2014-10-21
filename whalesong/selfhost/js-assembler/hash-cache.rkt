#lang racket/base

;; on-disk hashtable cache.

(require (prefix-in whalesong: "../version.rkt")
         racket/runtime-path
         racket/file
         file/md5)


(define cache-directory-path
  (build-path (find-system-path 'pref-dir)
              "whalesong"))

(provide cached? save-in-cache!)


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


(define whalesong-cache.scm
  (build-path cache-directory-path 
              (format "whalesong-cache-~a.scm"
                      whalesong:version)))


(define (ensure-cache-db-structure!)
  (when (not (file-exists? whalesong-cache.scm))
    ;; Clear existing cache files: they're obsolete.
    (clear-cache-files!)
    (call-with-output-file whalesong-cache.scm
      (lambda (op)
        (write (make-hash) op)))))
    


(define (get-db)
  (hash-copy (call-with-input-file whalesong-cache.scm read)))


(define (write-db! hash)
  (call-with-output-file whalesong-cache.scm
    (lambda (op) (write hash op))
    #:exists 'replace))




(create-cache-directory!)
(ensure-cache-db-structure!)
(define db (get-db))




;; cached?: path -> (U false bytes)
;; Returns a true value, (vector path md5-signature data), if we can
;; find an appropriate entry in the cache, and false otherwise.
(define (cached? path)
  (cond
    [(file-exists? path)
     (hash-ref db
               (list (path->string path)
                     (call-with-input-file* path md5))
               #f)]
    [else
     #f]))


;; save-in-cache!: path bytes -> void
;; Saves a record.
(define (save-in-cache! path data)
  (cond
    [(file-exists? path)
     (define signature (call-with-input-file* path md5))
     (hash-set! db
                (list (path->string path)
                      signature)
                data)
     (write-db! db)]
    [else
     (error 'save-in-cache! "File ~e does not exist" path)]))