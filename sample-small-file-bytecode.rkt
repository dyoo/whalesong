#lang racket
(require compiler/zo-parse)
(require (for-syntax racket/base))

(provide bytecode)


(define bytecode
  (parameterize ([current-namespace (make-base-namespace)])
  (let ([bc (compile (parameterize ([read-accept-reader #t])
                       (read (open-input-file "/home/dyoo/work/js-sicp-5-5/sample-small-file.rkt"))))]
        [op (open-output-bytes)])
    (write bc op)
    (zo-parse (open-input-bytes (get-output-bytes op))))))