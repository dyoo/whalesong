#lang racket/base
(require net/base64
         file/md5)

(provide munge-path)

;; munge-path: path -> string
;;
;; Given a path, gives a munged base path string.
(define (munge-path a-path)  
  (define encoding-prefix (let ([op (open-output-string)])
                            (base64-encode-stream (open-input-bytes
                                                   (md5 (path->string (build-path a-path))))
                                                  op
                                                  "")
                            (get-output-string op)))
  (define-values (base path dir?) (split-path a-path))
  (string-append encoding-prefix "_" (path->string path)))
