#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/image))

(define-resource whale-resource "images/humpback.jpg")
(define-resource self-resource "using-resources.rkt")

(define whale-image 
  (image-url
   (resource->url whale-resource)))

(list whale-image whale-image)
(resource? whale-image)
(image? whale-image)


(list whale-resource whale-resource)
(resource? whale-resource)
(image? whale-resource)


(list self-resource self-resource)
(resource? self-resource)
(image? self-resource)
