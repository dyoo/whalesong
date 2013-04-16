#lang racket/base

(require json
         file/gzip
         racket/runtime-path
         racket/port
         racket/match
         racket/pretty
         web-server/servlet-env
         web-server/servlet
         "repl-compile.rkt"         
         "modularize-input-port.rkt"
         "../make/make-structs.rkt"
         "../js-assembler/package.rkt"
         "../parser/parse-bytecode.rkt"
         "../compiler/compiler.rkt"
         "../js-assembler/assemble.rkt"
         (for-syntax racket/base))

(provide start-server)

(define-runtime-path htdocs (build-path "htdocs"))

(define language 
  'whalesong/wescheme/lang/semantics
  ;'whalesong/simply-scheme/semantics
  )



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


(define (lookup-binding req id)
  (if (exists-binding? 'id (request-bindings req))
      (extract-binding/single 'id (request-bindings req))
      #f))

(define (start req)
  (define-values (response op) 
    (make-port-response #:mime-type #"text/json" #:with-cors? #t))
  (define source-name (lookup-binding req 'name))
  (define mname (lookup-binding req 'mname))
  (define lang (lookup-binding req 'lang))
  (define src (extract-binding/single 'src (request-bindings req)))
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
           (define ip (open-input-string src))
           (port-count-lines! ip)
           (define assembled-codes
             (let loop () 
               (define sexp (read-syntax source-name ip))
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
           (define program-input-port
             (let* ([ip (open-input-string src)])
               (port-count-lines! ip)
               (modularize-input-port #:module-name (string->symbol mname)
                                      #:input-port ip
                                      #:semantics-module 'whalesong)))
           (define program-output-port (open-output-string))
           (package (SexpSource (parameterize ([read-accept-reader #t])
                                  (read-syntax source-name program-input-port)))
                    #:should-follow-children? (lambda (src) #f)
                    #:output-port  program-output-port)
           (write-json (hash 'type "module"
                             'module-name (string->symbol mname)
                             'provides '()  ;; FIXME!
                             'compiledModule (get-output-string program-output-port))
                       op)]))
  (close-output-port op)
  response)
  



(define (start-server #:port [port 8000]
                      #:listen-ip [listen-ip "127.0.0.1"])
    (thread (lambda ()
              (printf "starting web server on port ~s\n" port)
              (serve/servlet start 
                             #:listen-ip listen-ip
                             #:servlet-path "/compile"
                             #:extra-files-paths (list htdocs)
                             #:launch-browser? #f
                             #:port port))))

(module+ main
  (define current-port (make-parameter 8080))
  (require racket/cmdline)
  (void (command-line
         #:once-each 
         [("-p" "--port") p "Port (default 8000)" 
          (current-port (string->number p))]))
  (sync (start-server #:port (current-port))))
  
