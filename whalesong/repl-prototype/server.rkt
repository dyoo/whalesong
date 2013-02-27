#lang racket/base



(require json
         file/gzip
         racket/runtime-path
         racket/port
         racket/match
         web-server/servlet-env
         web-server/servlet
         "../make/make-structs.rkt"
         "../js-assembler/package.rkt"
         "write-runtime.rkt"
         (for-syntax racket/base))

(define-runtime-path htdocs (build-path "htdocs"))

;; make-port-response: (values response/incremental output-port)
;; Creates a response that's coupled to an output-port: whatever you
;; write into the output will be pushed into the response.
(define (make-port-response #:mime-type (mime-type #"application/octet-stream")
                            #:with-gzip? (with-gzip? #t))
  (define headers (if with-gzip?
                      (list (header #"Content-Encoding" #"gzip"))
                      (list)))
  (define-values (in out) (make-pipe))
  (values (response
             200 #"OK"
             (current-seconds)
             mime-type
             headers
             (lambda (op)
               (cond
                [with-gzip?
                 (gzip-through-ports in op #f (current-seconds))]
                [else
                 (copy-port in op)])))
            out))



(define (start req)
  (define-values (response op) 
    (make-port-response #:mime-type #"text/json"))
  (define text-src (extract-binding/single 'src (request-bindings req)))
  (define as-mod? (match (extract-bindings 'm (request-bindings req))
                    [(list (or "t" "true"))
                     #t]
                    [else #f]))
  ;; Compile the program here...
  (define program-port (open-output-string))
  (cond #;[(not as-mod?)
           ...]
        [else
         (package (SexpSource (parameterize ([read-accept-reader #t])
                                (read (open-input-string (string-append "#lang whalesong\n" text-src)))))
                  #:should-follow-children? (lambda (src) #f)
                  #:output-port  program-port)
         ])
  ;; Send it back as json text....
  (write-json (hash 'compiled (get-output-string program-port))
              op)
  (close-output-port op)
  (printf "done\n")
  response)
  


(write-repl-runtime-files)
(serve/servlet start 
               #:servlet-path "/compile"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:port 8080)