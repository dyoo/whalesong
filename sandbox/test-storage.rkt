#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/storage))

(storage-length)
(storage-ref "whalesong test")
(storage-set! "whalesong test" "hello world")
(storage-ref "whalesong test")
(storage-length)
(storage-clear!)
(storage-length)