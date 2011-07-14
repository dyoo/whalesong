#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/js))

(when (in-javascript-context?)
  (viewport-width))

(when (in-javascript-context?)
  (viewport-height))
