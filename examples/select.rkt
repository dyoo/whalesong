#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world))


(define view (xexp->view '(select (@ (id "my-select"))
                                  (option (@ (value "red")) "Red")
                                  (option (@ (value "green")) "Green")
                                  (option (@ (value "blue")) "Blue"))))
                                  


(big-bang 'undefined
          (initial-view view))