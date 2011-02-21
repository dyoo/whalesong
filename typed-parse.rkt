#lang typed/racket/base
(require "typed-structs.rkt")
(require/typed "parse.rkt"
               [parse (Any -> Expression)])

(provide parse)