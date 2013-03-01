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
         "../repl-compile.rkt"
         "../parser/parse-bytecode.rkt"
         "../compiler/compiler.rkt"
         "../compiler/compiler-structs.rkt"
         "../js-assembler/assemble.rkt"
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
  
  (cond [(not as-mod?)
         (define ip (open-input-string text-src))
         (port-count-lines! ip)
         (define assembled-codes
           (let loop () 
             (define sexp (read-syntax #f ip))
             (cond [(eof-object? sexp)
                    '()]
                   [else
                    (define raw-bytecode (repl-compile sexp #:lang 'whalesong/lang/whalesong))
                    (define op (open-output-bytes))
                    (write raw-bytecode op)
                    (define whalesong-bytecode (parse-bytecode (open-input-bytes (get-output-bytes op))))
                    (define compiled-bytecode (compile whalesong-bytecode 'val next-linkage/keep-multiple-on-stack))
                    (define assembled-op (open-output-string))
                    (define assembled (assemble/write-invoke compiled-bytecode #f assembled-op))
                    (cons (get-output-string assembled-op) (loop))])))
         (printf "assembled codes ~s\n" assembled-codes)
         (write-json (hash 'compiledCodes assembled-codes)
                     op)]
        [else
         (define program-port (open-output-string))
         (package (SexpSource (parameterize ([read-accept-reader #t])
                                (read (open-input-string (string-append "#lang whalesong\n" text-src)))))
                  #:should-follow-children? (lambda (src) #f)
                  #:output-port  program-port)
         (write-json (hash 'compiledModule (get-output-string program-port))
                     op)
         ])
  ;; Send it back as json text....

  (close-output-port op)
  (printf "done\n")
  response)
  


;(write-repl-runtime-files)
(serve/servlet start 
               #:servlet-path "/compile"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:port 8080)