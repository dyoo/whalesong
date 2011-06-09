#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))


;; insert-break: -> void
(define (insert-break)
  (call ($ "<br/>") "appendTo" body)
  (void))


(define (write-message msg)
  (void (call (call (call ($ "<span/>") "text" msg)
                    "css" "white-space" "pre")
              "appendTo"
              body)))



;; Set the background green.
(void (call body "css" "background-color" "lightgreen"))
(void (call ($ "<h1>Hello World</h1>") "appendTo" body))
(write-message "Hello, this is a test!")
(insert-break)
(let loop ([i 0])
  (cond
   [(= i 10)
    (void)]
   [else
    (write-message "iteration ") (write-message i)
    (insert-break)
    (loop (add1 i))]))

(write-message "viewport-width: ") (write-message (viewport-width))
(insert-break)
(write-message "viewport-height: ") (write-message (viewport-height))
(insert-break)