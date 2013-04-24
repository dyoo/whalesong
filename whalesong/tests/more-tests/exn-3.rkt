#lang whalesong

(with-handlers ([(lambda (exn) #f)
                   (lambda (exn) +inf.0)])
    (car 17))
