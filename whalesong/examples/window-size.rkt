#lang whalesong

(require whalesong/js)

(when (in-javascript-context?)
  (viewport-width))

(when (in-javascript-context?)
  (viewport-height))
