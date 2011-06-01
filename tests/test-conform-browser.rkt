#lang racket
(require "browser-evaluate.rkt"
         "../js-assembler/package.rkt"
         "../make-structs.rkt"
         racket/port
         racket/runtime-path)

(printf "test-conform-browser.rkt\n")

(define-runtime-path conform-path (build-path "conform"))


(define evaluate (make-evaluate 
                  (lambda (program op)

                    (fprintf op "(function () {")
                    
                    (displayln (get-runtime) op)
                    
                    (newline op)
                    
                    (fprintf op "var innerInvoke = ")
                    (package-anonymous (make-SexpSource program)
                                       #:should-follow? (lambda (p) #t)
                                       #:output-port op)
                    (fprintf op "();\n")
                    
                    (fprintf op #<<EOF
return (function(succ, fail, params) {
            return innerInvoke(new plt.runtime.Machine(), succ, fail, params);
        });
});
EOF
                             )
                    
                    )))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ s exp)
     (with-syntax ([stx stx])
       (syntax/loc #'stx
         (begin
           (printf "running test...")
           (let ([result (evaluate s)])
             (let ([output (evaluated-stdout result)])
               (unless (string=? output exp)
                 (printf " error!\n")
                 (raise-syntax-error #f (format "Expected ~s, got ~s" exp output)
                                     #'stx)))
             (printf " ok (~a milliseconds)\n" (evaluated-t result))))))]))


(test (read (open-input-file (build-path conform-path "program0.sch")))
      (port->string (open-input-file (build-path conform-path "expected0.txt"))))