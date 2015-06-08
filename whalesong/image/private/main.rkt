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
                    
                    bitmap/url
                    image-url      ;; older name for bitmap/url
                    open-image-url ;; older name for bitmap/url
                    video/url
                    play-sound
                    overlay
                    overlay/offset
                    overlay/xy
                    overlay/align
                    underlay
                    underlay/offset
                    underlay/xy
                    underlay/align
                    beside
                    beside/align
                    above
                    above/align
                    empty-scene
                    put-image
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
                    polygon
                    regular-polygon
                    ellipse
                    triangle
                    triangle/sas
                    triangle/sss
                    triangle/ass
                    triangle/ssa
                    triangle/aas
                    triangle/asa
                    triangle/saa
                    right-triangle
                    isosceles-triangle
                    star
                    radial-star
                    star-polygon
                    rhombus
                    image->color-list
                    color-list->image
                    color-list->bitmap
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
                    image?
                    image=?
                    name->color
                    ))
