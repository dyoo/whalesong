#lang whalesong


(with-handlers ([exn:fail?
                 (lambda (exn)
                   "nested catch")])
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (raise exn))])
    (/ 1 0)))
