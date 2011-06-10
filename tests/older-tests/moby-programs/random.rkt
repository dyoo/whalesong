#lang s-exp "../../lang/wescheme.rkt"
(require "../../lang/check-expect/test-expect.rkt")
"random.rkt"


(let loop ([i 0])
  (when (< i 1000)
    (begin
      (test-within (random 100) 50 50)
      (loop (add1 i)))))

(let loop ([i 0])
  (when (< i 1000)
    (begin
      (test-within (random) 0.5 0.5)
      (loop (add1 i)))))

"random.rkt end"