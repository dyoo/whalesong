#lang typed/racket/base
(require "expression-structs.rkt")
(require/typed "parse.rkt"
               [parse (Any -> Expression)])

(provide parse)