#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))


;; insert-break: -> void
(define (insert-break)
  (call ($ "<br/>") "appendTo" body)
  (void))


(void (call ($ "<h1>Hello world</h1>") "appendTo" body))


(void (call body "append" "hello, this is a test"))
(insert-break)
(void (call body "append" "hello, this is a test"))
(insert-break)
(void (call body "css" "background-color" "lightgreen"))



(void (call ($ "<span>This is another thing that has been added.</span>")
            "appendTo"
            body))
