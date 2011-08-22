#lang s-exp "../lang/kernel.rkt"

(provide (all-defined-out))

;; Needs to be prefabricated
(struct resource (path key content) #:prefab)
