#lang planet dyoo/whalesong
(require (planet dyoo/whalesong/image)
         (planet dyoo/whalesong/resource))


;; color-near? : Color Color Number -> Boolean
;; Is the first color within tolerance of the second?
(define (color-near? a b tolerance)
  (and (< (abs (- (color-red   a) (color-red   b))) tolerance)
       (< (abs (- (color-green a) (color-green b))) tolerance)
       (< (abs (- (color-blue  a) (color-blue  b))) tolerance)
       (< (abs (- (color-alpha a) (color-alpha b))) tolerance)))

;; color=? : Color Color -> Boolean
;; Is the first color the same as the second?
(define (color=? a b)
  (and a b 
       (equal? (color-red   a) (color-red   b))
       (equal? (color-green a) (color-green b))
       (equal? (color-blue  a) (color-blue  b))
       (equal? (color-alpha a) (color-alpha b))))

(define (imgvec-location x y w h)
  (+ (* y w) x))

(define (imgvec-adjacent-points imgvec loc width height)
  (let ((x (remainder loc width))
        (y (floor (/ loc width)))
        (vloc (lambda (x y) (imgvec-location x y width height))))
    (append
     (if (< 0 x)            (list (vloc (- x 1)    y   )) '())
     (if (< 0 y)            (list (vloc    x    (- y 1))) '())
     (if (< x (- width 1))  (list (vloc (+ x 1)    y   )) '())
     (if (< y (- height 1)) (list (vloc    x    (+ y 1))) '()))))

(define (color-connected-points! imgvec width height start-color destination-color tolerance it)
  (let ((mycol (vector-ref imgvec it)))
    (printf "~a ~a\n" mycol it)
    (when (and (not (color=? mycol destination-color))
               (color-near? mycol start-color tolerance))
      (begin
        (vector-set! imgvec it destination-color)
        (for-each (lambda (loc) 
                    (color-connected-points! 
                     imgvec width height start-color destination-color tolerance loc))
                  (imgvec-adjacent-points imgvec it width height))))))

(define (fill-from-point! img start-x start-y source-color destination-color tolerance dust-size)
  (let* ((v (list->vector (image->color-list img)))
         (width (image-width img))
         (height (image-height img))
         (c (if source-color
                (name->color source-color)
                (vector-ref v (imgvec-location start-x start-y width height))))
         (d (if (string? destination-color) (name->color destination-color) destination-color)))
    (begin
      (when (not (color=? c d))
        (color-connected-points! v width height c d tolerance
                                 (imgvec-location start-x start-y width height)))
      (color-list->bitmap (vector->list v) width height))))

(define (transparent-from-corner img tolerance)
  (fill-from-point! img 0 0 #f (make-color 0 0 0 0) tolerance 0))

(define (transparent-from-corners img tolerance)
  (let ((xprt (make-color 0 0 0 0))
        (start-color #f)
        (jaggies 0)
        (w-1 (- (image-width img) 1))
        (h-1 (- (image-height img) 1)))
    (fill-from-point!
     (fill-from-point!
      (fill-from-point!
       (fill-from-point! img 0 0 start-color xprt tolerance jaggies)
       w-1 0 start-color xprt tolerance jaggies)
      0 h-1 start-color xprt tolerance jaggies)
     w-1 h-1 start-color xprt tolerance jaggies)))

;; replace-color : Image Color Color Number -> Image
;; In the given image, replace the source color (with the given tolerance)
;; by the destination color
(define (replace-color img source-color destination-color tolerance)
  (let ((src (name->color source-color))
        (dst (name->color destination-color)))
    (color-list->bitmap
     (map (lambda (c)
            (if (color-near? c src tolerance)
                dst
                c))
          (image->color-list img))
     (image-width img)
     (image-height img))))

;; color->alpha : Image Color Number -> Image
;; in the given image, transform the given color to transparency.
(define (color->alpha img target-color tolerance)
  (replace-color img target-color (make-color 0 0 0 0) tolerance))

;; clipart-url : String -> Image
;; try to grab the provided url and turn it into an image assuming a solid white background
(define (clipart/url url)
  (transparent-from-corners (bitmap/url url) 30))

(define (time name thunk)
  (let* ((start (current-seconds))
         (result (thunk))
         (elapsed (- (current-seconds) start)))
    (begin
      (display "Ran ") (display name) (display " in ") (display elapsed) (display " seconds.") (newline)
      result)))

(define BG (rectangle 300 100 "solid" "green"))
(define-resource dog.jpg)   ;;  "http://t3.gstatic.com/images?q=tbn:ANd9GcSiCx-eVMoU6wpH2WgfNzOTd_wZunA-S07ZZJsGtHiKNfOUp2chMKmvEVajtg")
(define DOG (scale 1/2 dog.jpg))
                                        ;(define XDOG (time "(transparent-from-corners DOG 30)" (lambda () (transparent-from-corners DOG 30))))
                                        ;(define D (overlay XDOG BG))
                                        ;(define CDOG (overlay (clipart/url DOGURL) BG))
                                        ;D

(define colors (image->color-list DOG))
(displayln (length colors))
(andmap color? colors)
(define vec (list->vector colors))
(vector-length vec)
(let loop ([i 0])
  (when (< i (vector-length vec))
    (unless (color? (vector-ref vec i))
      (error 'something-went-wrong))
    (loop (add1 i))))


(define (repeat num thunk)
  (if (equal? num 0)
      (thunk)
      (begin
        (thunk)
        (repeat (- num 1) thunk))))
(time "(transparent-from-corners DOG 30)" (lambda () (transparent-from-corners DOG 30)))
;; Ran (transparent-from-corners DOG 30) in 7 (sometimes 8) seconds.
(define v (time "just list->vector image->color-list" (lambda () (list->vector (image->color-list DOG)))))
;; Ran just list->vector image->color-list in 0 seconds.
(define DOG-many (vector-length v))
(time (string-append (number->string DOG-many) " imgvec-adjacent-points")
      (lambda () (repeat DOG-many (lambda () (imgvec-adjacent-points v (imgvec-location 41 41 45 45) 45 45)))))
;; Ran 3136 imgvec-adjacent-points in 3 seconds.
(time (string-append (number->string (* 4 DOG-many)) " imgvec-locations")
      (lambda () (repeat (* 4 DOG-many) (lambda () (imgvec-location 41 41 45 45)))))
;; Ran 3136 imgvec-locations in 1 seconds.
(time (string-append (number->string (* 4 DOG-many)) " additions")
      (lambda () (repeat (* 4 DOG-many) (lambda () (+ 4 5)))))
(time (string-append (number->string (* 4 DOG-many)) " multiplies")
      (lambda () (repeat (* 4 DOG-many) (lambda () (* 4 5)))))
(time (string-append (number->string (* 4 DOG-many)) " function calls")
      (lambda () (repeat (* 4 DOG-many) (lambda () #t))))
;; Ran 3136 function calls in 0 seconds.

;; at least 2 out of 10 seconds are being spent only on adding and multiplying?
;; but that means that 3 seconds are being used in function calls, and I can just inline those... painful, but perhaps effective.

