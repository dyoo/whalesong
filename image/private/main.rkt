#lang s-exp "../../lang/js/js.rkt"

;; We need to make sure the color module has been invoked
;; before invoking this module, since the JavaScript implementation
;; depends on it.
(require "color.rkt")

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("colordb.js"
               "kernel.js"
               "js-impl.js")
 #:provided-values (text
                    text/font
                    image-url
                    open-image-url
                    overlay
                    overlay/xy
                    overlay/align
                    underlay
                    underlay/xy
                    underlay/align
                    beside
                    beside/align
                    above
                    above/align
                    empty-scene
                    place-image
                    place-image/align
                    rotate
                    scale
                    scale/xy
                    flip-horizontal
                    flip-vertical
                    frame
                    crop
                    line
                    add-line
                    scene+line
                    circle
                    square
                    rectangle
                    regular-polygon
                    ellipse
                    triangle
                    right-triangle
                    isosceles-triangle
                    star
                    radial-star
                    star-polygon
                    rhombus
                    image->color-list
                    color-list->image
                    image-width
                    image-height
                    image-baseline
                    image-color?
                    mode?
                    x-place?
                    y-place?
                    angle?
                    side-count?
                    step-count?
                    image?))
