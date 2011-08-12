#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/resource))
(define-resource whale-resource "images/humpback.jpg")

#;(define whale-image 
    (image-url
     (resource->url whale-resource)))

whale-resource
