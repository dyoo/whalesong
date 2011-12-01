#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/storage))

(storage-length)
(storage-ref "whalesong test")
(storage-set! "whalesong test" "hello world")
(storage-ref "whalesong test")
(storage-length)
(storage-key 0)
(storage-clear!)
(storage-length)


(storage-clear!)
(storage-set! "name" "Danny")
(storage-set! "advisor" "sk")
(storage-set! "advisor" "kathi")
(storage-length)
(storage-key 0)
(storage-key 1)
(storage-remove! "advisor")
(storage-length)
(storage-key 0)
(storage-remove! "name")
(storage-length)
(storage-clear!)