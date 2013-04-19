#lang whalesong

(with-handlers ([exn:fail?
                 (lambda (exn)
                   (printf "I see ~s\n" (exn-message exn)))])
  (/ 1 0))
