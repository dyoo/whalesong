#lang whalesong
(with-handlers ([exn:fail:contract?
                   (lambda (exn) +inf.0)])
    (/ 1 0))
