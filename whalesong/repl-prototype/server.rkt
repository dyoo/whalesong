#lang racket/base



(require json
         file/gzip
         racket/runtime-path
         racket/port
         web-server/servlet-env
         web-server/servlet
         "write-runtime.rkt"
         (for-syntax racket/base))

(define-runtime-path htdocs (build-path "htdocs"))

;; make-port-response: (values response/incremental output-port)
;; Creates a response that's coupled to an output-port: whatever you
;; write into the output will be pushed into the response.
(define (make-port-response #:mime-type (mime-type #"application/octet-stream")
                            #:with-gzip? (with-gzip? #f))
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
  (define source (extract-binding/single 'src (request-bindings req)))
  ;; Compile the program here...
  ;; Send it back as json text....
  (write-json '(1 2 3) op)
  (close-output-port op)
  response)
  


(write-repl-runtime-files)
(serve/servlet start 
               #:servlet-path "/compile"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:port 8080)