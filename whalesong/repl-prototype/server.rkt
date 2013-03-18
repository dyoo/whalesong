#lang racket/base

(require "repl-compile.rkt"
         json
         file/gzip
         racket/runtime-path
         racket/port
         racket/match
         racket/pretty
         racket/cmdline
         web-server/servlet-env
         web-server/servlet
         "../make/make-structs.rkt"
         "../js-assembler/package.rkt"
         "../parser/parse-bytecode.rkt"
         "../compiler/compiler.rkt"
         "../js-assembler/assemble.rkt"
         "write-runtime.rkt"
         (for-syntax racket/base))

(define-runtime-path htdocs (build-path "htdocs"))

(define language 'whalesong/wescheme/lang/semantics)



;; make-port-response: (values response/incremental output-port)
;; Creates a response that's coupled to an output-port: whatever you
;; write into the output will be pushed into the response.
(define (make-port-response #:mime-type (mime-type #"application/octet-stream")
                            #:with-gzip? (with-gzip? #t)
                            #:with-cors? (with-cors? #f))
  (define headers 
    (filter values 
            (list (if with-gzip?
                      (header #"Content-Encoding" #"gzip")
                      #f)
                  (cond [(not with-cors?) 
                         #f]
                        [(bytes? with-cors?)
                         (header #"Access-Control-Allow-Origin" with-cors?)]
                        [(eq? with-cors? #t)
                         (header #"Access-Control-Allow-Origin" #"*")]
                        [else
                         (raise-argument-error 'make-port-response
                                               "byte string or boolean"
                                               with-cors?)]))))
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
    (make-port-response #:mime-type #"text/json" #:with-cors? #t))
  (define name (if (exists-binding? 'name (request-bindings req))
                   (extract-binding/single 'name (request-bindings req))
                   #f))
  (define text-src (extract-binding/single 'src (request-bindings req)))
  (define as-mod? (match (extract-bindings 'm (request-bindings req))
                    [(list (or "t" "true"))
                     #t]
                    [else #f]))
  ;; Compile the program here...
  (with-handlers ([exn:fail? (lambda (exn)
                               (write-json (hash 'type "error"
                                                 'message (exn-message exn))
                                           op))])
    (cond [(not as-mod?)
           (define ip (open-input-string text-src))
           (port-count-lines! ip)
           (define assembled-codes
             (let loop () 
               (define sexp (read-syntax name ip))
               (cond [(eof-object? sexp)
                      '()]
                     [else
                      (define raw-bytecode (repl-compile sexp #:lang language))
                      (define op (open-output-bytes))
                      (write raw-bytecode op)
                      (define whalesong-bytecode (parse-bytecode (open-input-bytes (get-output-bytes op))))
                      #;(pretty-print whalesong-bytecode)
                      (define compiled-bytecode (compile-for-repl whalesong-bytecode))
                      #;(pretty-print compiled-bytecode)
                      (define assembled-op (open-output-string))
                      (define assembled (assemble/write-invoke compiled-bytecode assembled-op 'with-preemption))
                      (cons (get-output-string assembled-op) (loop))])))
           #;(printf "assembled codes ~a\n" assembled-codes)
           (write-json (hash 'type "repl"
                             'compiledCodes assembled-codes)
                       op)]
          [else
           (define program-port (open-output-string))
           (package (SexpSource (parameterize ([read-accept-reader #t])
                                  (read (open-input-string (string-append "#lang whalesong\n" text-src)))))
                    #:should-follow-children? (lambda (src) #f)
                    #:output-port  program-port)
           (write-json (hash 'type "module"
                             'compiledModule (get-output-string program-port))
                       op)
           ]))
  ;; Send it back as json text....

  (close-output-port op)
  response)
  


(define current-port (make-parameter 8080))
(void (command-line
       #:once-each 
       [("-p" "--port") p "Port (default 8080)" 
                        (current-port (string->number p))]))


(write-repl-runtime-files)
(serve/servlet start 
               #:servlet-path "/compile"
               #:extra-files-paths (list htdocs)
               #:launch-browser? #f
               #:port (current-port))
