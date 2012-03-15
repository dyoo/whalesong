#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/web-world)
         "geo.rkt")


(big-bang (list 'undefined 'undefined)
          (on-geo (lambda (w v lat lng)
                    (list lat lng))))