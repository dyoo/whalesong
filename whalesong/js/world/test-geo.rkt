#lang whalesong

(require whalesong/web-world
         "geo.rkt")


(big-bang (list 'undefined 'undefined)
          (on-geo (lambda (w v lat lng)
                    (list lat lng))))
