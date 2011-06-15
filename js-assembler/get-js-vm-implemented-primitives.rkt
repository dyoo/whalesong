#lang racket/base

(require racket/runtime-path
         racket/file
         racket/contract
         racket/list)
;; Get the list of primitives implemented in js-vm-primitives.js

(define-runtime-path js-vm-primitives.js "runtime-src/js-vm-primitives.js")

(define-runtime-path whalesong-primitives.js "runtime-src/runtime.js")

;; sort&unique: (listof string) -> (listof string)
(define (sort&unique names)
  (let ([ht (make-hash)])
    (for ([name names])
      (hash-set! ht name #t))
    (sort (for/list ([name (in-hash-keys ht)])
            name)
          string<?)))
        
;; primitive-names: (listof symbol)
(define js-vm-primitive-names
  (map string->symbol
       (sort&unique
        (map (lambda (a-str)
               (substring a-str
                          (string-length "PRIMITIVES['")
                          (- (string-length a-str) (string-length "']"))))
             (let ([contents (file->string js-vm-primitives.js)])
               (regexp-match* #px"PRIMITIVES\\[('|\")[^\\]]*('|\")\\]" contents))))))



(define whalesong-primitive-names
  (map string->symbol
       (sort&unique
        (map (lambda (a-str)
               (let ([match (regexp-match
                             #px"installPrimitiveProcedure\\(\\s+('|\")([^\\]]*)('|\")" a-str)])
                 (third match)))
             (let ([contents (file->string whalesong-primitives.js)])
               (regexp-match* #px"installPrimitiveProcedure\\(\\s+('|\")[^\\']*('|\")" contents))))))

     
(provide/contract [js-vm-primitive-names (listof symbol?)]
                  [whalesong-primitive-names (listof symbol?)])