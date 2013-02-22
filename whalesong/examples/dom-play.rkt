#lang whalesong

(require whalesong/js)


;; insert-break: -> void
(define (insert-break)
  (call-method ($ "<br/>") "appendTo" body)
  (void))


(define (write-message msg)
  (void (call-method (call-method (call-method ($ "<span/>") "text" msg)
                    "css" "white-space" "pre")
              "appendTo"
              body)))



;; Set the background green.
(void (call-method body "css" "background-color" "lightgreen"))
(void (call-method ($ "<h1>Hello World</h1>") "appendTo" body))
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
