#lang whalesong

(current-print-mode "constructor")


'(1 2 3)
(list "hello" "world")
(list 'hello 'world)
(cons 1 2)
(cons 1 (cons 2 (cons 3 empty)))
(cons 1 (cons 2 (cons 3 4)))
'()

'hello
(box 'hello)
(vector 'hello 'world)

(define-struct person (name age))
(person 'danny 32)
(person "jerry" 32)


;; This is slightly broken: we should follow DrRacket shared printing
;; notation.
(shared ([a (cons 1 a)])
  a)
