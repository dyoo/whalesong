#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/image))

(define-resource whale-resource "images/humpback.jpg")

(define whale-image 
  (image-url
   (resource->url whale-resource)))

whale-resource
whale-image
whale-image