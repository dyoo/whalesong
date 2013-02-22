#lang whalesong
(define program (lambda () (let ((y (call/cc (lambda (c) c)))) (display 1) (call/cc (lambda (c) (y c))) (display 2) (call/cc (lambda (c) (y c))) (display 3))))

(program)
(newline)
