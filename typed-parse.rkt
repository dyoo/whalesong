#lang typed/racket/base
(require "expression-structs.rkt")
(require/typed "parse.rkt"
               [parse (Any -> ExpressionCore)])

(provide parse)