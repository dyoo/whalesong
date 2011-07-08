#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/image))

(printf "images.rkt\n")


;; (check-expect (image? 'blue) #f)
;; (check-expect (image? (circle 20 "solid" "green")) #t)

;; "should be a solid green circle: " (circle 20 "solid" "green")


;; (check-expect (image=? (circle 50 "solid" "blue")
;;            (rectangle 20 30 "outline" "turquoise"))
;;         #f)
;; "should be an outline turquoise rectangle: " (rectangle 20 30 "outline" "turquoise")


;; ;(check-expect (color? (make-color 3 4 5)))

;; (check-expect (color-red (make-color 3 4 5)) 3)
;; (check-expect (color-green (make-color 3 4 5)) 4)
;; (check-expect (color-blue (make-color 3 4 5)) 5)

;; (check-expect (image? (empty-scene 20 50)) true)
;; (check-expect (image=? (empty-scene 20 50) (empty-scene 20 50)) true)

;; (check-expect (image? (place-image (circle 50 'solid 'blue)
;;            50
;;            50
;;            (empty-scene 100 100)))
;;         true)

;; "should be a blue circle in a scene with a border: " (place-image (circle 50 'solid 'blue)
;;            50
;;            50
;;            (empty-scene 100 100))

;; (check-expect (image?
;;          (put-pinhole (rectangle 20 20 'solid 'green) 0 0))
;;         true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT & TEXT/FONT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"simple text functionality" 
(text "hello world" 20 'black)
(text (string-copy "hello world") 30 'purple)
(text "hello world" 40 'red)


"test font-weight"
(text/font "Hello" 24 "purple"
               "Gill Sans" 'swiss 'normal 'bold #f)
(text/font "Hello" 24 "green"
               "Gill Sans" 'swiss 'normal 'light #f)
  
"test font-style"
(text/font "Goodbye" 48 "indigo"
               "Helvetica" 'modern 'italic 'normal #f)
(text/font "Goodbye" 48 "indigo"
               "Helvetica" 'modern 'normal 'normal #f)

"test underline-height calculation"
(text/font "test this!" 80 "purple"
              "Helvetica" 'roman 'normal 'normal #t)
        
(text/font "low-hanging glyphs" 36 "blue"
              "Times" 'roman 'normal 'bold #t)  

(text/font "teeny-tiny text" 8 "black"
              "Times" 'roman 'normal 'bold #t)  

(text/font "not really a link" 36 "blue"
              "Courier" 'roman 'italic 'normal #t)
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE-URL & VIDEO-URL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"importing images and video"
(image-url "http://racket-lang.org/logo.png")
(open-image-url "http://racket-lang.org/logo.png")

;(video-url "http://www.youtube.com/demo/google_main.mp4")
;(overlay (circle 20 "solid" "red")
;  (video-url "http://www.youtube.com/demo/google_main.mp4"))
;(rotate 45
;  (video-url "http://www.youtube.com/demo/google_main.mp4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"the next two images should be identical"
(overlay (circle 20 "solid" (make-color  50  50 255))
             (square 40 "solid" (make-color 100 100 255)))

(overlay (circle 20 "solid" (make-color  50  50 255))
             (regular-polygon 40 4 "solid" (make-color 100 100 255)))

(overlay (ellipse 10 10 "solid" "red")
             (ellipse 20 20 "solid" "black")
             (ellipse 30 30 "solid" "red")
             (ellipse 40 40 "solid" "black")
             (ellipse 50 50 "solid" "red")
             (ellipse 60 60 "solid" "black"))

"the next two images should be identical"
(overlay (square 20 "solid" (make-color  50  50 255))
             (square 26 "solid" (make-color 100 100 255))
             (square 32 "solid" (make-color 150 150 255))
             (square 38 "solid" (make-color 200 200 255))
             (square 44 "solid" (make-color 250 250 255)))
(overlay (regular-polygon 20 4 "solid" (make-color  50  50 255))
             (regular-polygon 26 4 "solid" (make-color 100 100 255))
             (regular-polygon 32 4 "solid" (make-color 150 150 255))
             (regular-polygon 38 4 "solid" (make-color 200 200 255))
             (regular-polygon 44 4 "solid" (make-color 250 250 255)))
       
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERLAY/XY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"should be some overlay/xys"
(overlay/xy (rectangle 20 20 "outline" "black")
                20 0
                (rectangle 20 20 "outline" "black"))
(overlay/xy (rectangle 20 20 "solid" "red")
                20 20
                (rectangle 20 20 "solid" "black"))
(overlay/xy (rectangle 20 20 "solid" "red")
                -20 -20
                (rectangle 20 20 "solid" "black"))
(overlay/xy
     (overlay/xy (ellipse 40 40 "outline" "black")
                 10
                 15
                 (ellipse 10 10 "solid" "forestgreen"))
     20
     15
     (ellipse 10 10 "solid" "forestgreen"))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVERLAY/ALIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"some examples of overlay/align"
(overlay/align "middle" "middle"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(overlay/align "right" "top"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(overlay/align "left" "bottom"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
           
(overlay/align "right" "bottom"
                   (rectangle 20 20 "solid" "silver")
                   (rectangle 30 30 "solid" "seagreen")
                   (rectangle 40 40 "solid" "silver")
                   (rectangle 50 50 "solid" "seagreen"))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDERLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"some underlays"
(underlay (circle 20 'solid 'green)
    (rectangle 10 20 'solid 'blue))
    
(underlay (ellipse 10 60 "solid" "red")
              (ellipse 20 50 "solid" "black")
              (ellipse 30 40 "solid" "red")
              (ellipse 40 30 "solid" "black")
              (ellipse 50 20 "solid" "red")
              (ellipse 60 10 "solid" "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDERLAY/XY & UNDERLAY/ALIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"examples of underlay and underlay/align"
(underlay/xy (circle 20 'solid 'green)
       30 10
       (rectangle 10 20 'solid 'blue))



;; color list
"the following should be a blue circle, but by using color-list->image"
(let ([circle-color-list (image->color-list (circle 20 'solid 'blue))])
  ;; fixme: add tests for number of colors
  (color-list->image circle-color-list 40 40 0 0))




(underlay/align "middle" "middle"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(underlay/align "right" "top"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
(underlay/align "left" "bottom"
                   (ellipse 60 30 "solid" "purple")
                   (rectangle 30 60 "solid" "orange"))
           
(underlay/align "right" "bottom"
                   (rectangle 50 50 "solid" "silver")
                   (rectangle 40 40 "solid" "seagreen")
                   (rectangle 30 30 "solid" "silver")
                   (rectangle 20 20 "solid" "seagreen"))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BESIDE & BESIDE/ALIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"examples of beside and beside/align"
(beside (ellipse 20 70 "solid" "gray")
            (ellipse 20 50 "solid" "darkgray")
            (ellipse 20 30 "solid" "dimgray")
            (ellipse 20 10 "solid" "black"))
      
(beside/align "bottom"
                  (ellipse 20 70 "solid" "lightsteelblue")
                  (ellipse 20 50 "solid" "mediumslateblue")
                  (ellipse 20 30 "solid" "slateblue")
                  (ellipse 20 10 "solid" "navy"))
          
(beside/align "top"
                  (ellipse 20 70 "solid" "mediumorchid")
                  (ellipse 20 50 "solid" "darkorchid")
                  (ellipse 20 30 "solid" "purple")
                  (ellipse 20 10 "solid" "indigo"))

"align these text images on their baselines"
(beside/align "baseline"
                  (text "ijy" 18 "black")
                  (text "ijy" 24 "black"))               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABOVE & ABOVE/ALIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"should be some examples of above and above/align"
(above (ellipse 70 20 "solid" "gray")
           (ellipse 50 20 "solid" "darkgray")
           (ellipse 30 20 "solid" "dimgray")
           (ellipse 10 20 "solid" "black"))
                    
(above/align "right"
                 (ellipse 70 20 "solid" "gold")
                 (ellipse 50 20 "solid" "goldenrod")
                 (ellipse 30 20 "solid" "darkgoldenrod")
                 (ellipse 10 20 "solid" "sienna"))
(above/align "left"
                 (ellipse 70 20 "solid" "yellowgreen")
                 (ellipse 50 20 "solid" "olivedrab")
                 (ellipse 30 20 "solid" "darkolivegreen")
                 (ellipse 10 20 "solid" "darkgreen"))
        
                 
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLACE-IMAGE/ALIGN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"should be right in the center"
(place-image/align (circle 16 "solid" "yellow")
                       32 32 "center" "center"
                       (rectangle 64 64 "solid" "goldenrod"))

"should be at the bottom-right corner"
(place-image/align (circle 16 "solid" "yellow")
                       32 32 "left" "top"
                       (rectangle 64 64 "solid" "goldenrod"))

"should be at the upper-left corner"
(place-image/align (circle 16 "solid" "yellow")
                       32 32 "right" "bottom"
                       (rectangle 64 64 "solid" "goldenrod"))  
         
"test 'beside' with scenes -- from the DrRacket documentation"
(beside (place-image/align (circle 8 "solid" "tomato")
        0 0 "center" "center"
        (rectangle 32 32 "outline" "black"))
    (place-image/align (circle 8 "solid" "tomato")
            8 8 "center" "center"
            (rectangle 32 32 "outline" "black"))
    (place-image/align (circle 8 "solid" "tomato")
            16 16 "center" "center"
            (rectangle 32 32 "outline" "black"))
    (place-image/align (circle 8 "solid" "tomato")
            24 24 "center" "center"
            (rectangle 32 32 "outline" "black"))
    (place-image/align (circle 8 "solid" "tomato")
            32 32 "center" "center"
            (rectangle 32 32 "outline" "black")))   
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRIANGLE, RIGHT TRIANGLE & ISOSCELES-TRIANGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Three right triangles of various sizes and fills"
(triangle 36 "solid" "darkslategray")
(triangle  4 "solid" "purple")
(triangle 30 "solid" "cornflowerblue")

"Triangles side by side"
(beside (triangle 36 "solid" "darkslategray")
        (triangle 30 "solid" "cornflowerblue"))

"Triangles above."
(above (triangle 36 "solid" "darkslategray")
       (triangle 30 "solid" "cornflowerblue"))

"Three right triangles of various sizes and fills"
(right-triangle 36 48 "solid" "darkslategray")
(right-triangle  4 60 "solid" "purple")
(right-triangle 30 40 "solid" "cornflowerblue")

"Three isosceles triangles of various sizes and fills"

(isosceles-triangle 60 30 "solid" "aquamarine")
(isosceles-triangle 200 170 "outline" "seagreen")
(isosceles-triangle 60 330 "solid" "lightseagreen")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAR, RADIAL-STAR & STAR-POLYGON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"old star implementation"

(star 5  8  4 "solid"   "darkslategray")
(star 5 30 15 "outline" "black")
(star 5 20 10 "solid"   "red")

"new star implementation"
(star 8 "solid"    "darkslategray")
(star 30 "outline" "black")
(star 20 "solid"   "red")

"radial star"
(radial-star 8 8 64 "solid" "darkslategray")
(radial-star 32 30 40 "outline" "black")
(radial-star 5 20 40 "solid" "red")

"star-polygon"
(star-polygon 40 5 2 "solid" "seagreen")
(star-polygon 40 7 3 "outline" "darkred")
(star-polygon 20 10 3 "solid" "cornflowerblue")
"should look like a pentagon"
(star-polygon 20 5 1 "solid" "darkblue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQUARE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Three squares of various sizes and fills"
(square 60 "outline" "black")
(square 200 "solid" "seagreen")
(square 100 "outline" "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RHOMBUS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Three rhombuses of various sizes and fills"
(rhombus 40 45 "solid" "magenta")
(rhombus 100 200 "solid" "orange")
(rhombus 80 330 "outline" "seagreen")

"rhombuses beside each other"
(beside (rhombus 40 45 "solid" "magenta")
  (rhombus 100 200 "solid" "orange")
  (rhombus 80 330 "outline" "seagreen"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGULAR-POLYGON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Some regular polygons..."
"A triangle of side-length 20: should be 20x17"
(regular-polygon 20 3 "solid" "purple")
"A square of side-length 40: should be 40x40"
(regular-polygon 40 4 "solid" "aquamarine")
"A pentagon of side-length 30: should be 49x46"
(regular-polygon 30 5 "solid" "pink")
"A hexagon of side-length 20: should be 40x35"
(regular-polygon 20 6 "solid" "gold")
"A septagon of side-length 40: should be 90x88"
(regular-polygon 40 7 "solid" "goldenrod")
"An octagon of side-length 30: should be 72x72"
(regular-polygon 30 8 "solid" "darkgoldenrod")
"A nonagon of side-length 20: should be 58x57"
(regular-polygon 20 9 "solid" "sienna")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROTATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Three images at 30, 60, 90 degree rotation:"
(rotate 30 (image-url "http://racket-lang.org/logo.png"))
(rotate 60 (image-url "http://racket-lang.org/logo.png"))
(rotate 90 (image-url "http://racket-lang.org/logo.png"))

"Rotated, huge image"
(rotate 30 (scale 3 (image-url "http://racket-lang.org/logo.png")))

"From the Racket documentation:"
(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
(rotate 5 (rectangle 50 50 "outline" "black"))
(rotate 45
            (beside/align
             "center"
             (rectangle 40 20 "solid" "darkseagreen")
             (rectangle 20 100 "solid" "darkseagreen")))
          
(beside
     (rotate 30 (square 50 "solid" "red"))
     (flip-horizontal
      (rotate 30 (square 50 "solid" "blue"))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCALE & SCALE/XY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
"scaling small and large"
(scale 1/2 (image-url "http://racket-lang.org/logo.png"))
(scale 2 (image-url "http://racket-lang.org/logo.png"))

(scale/xy 1 2 (image-url "http://racket-lang.org/logo.png"))
(scale/xy 2 1 (image-url "http://racket-lang.org/logo.png"))

"This should be the normal image"
(scale/xy 1 1 (image-url "http://racket-lang.org/logo.png"))

"From the Racket documentation: two identical ellipses, and a circle"
(scale 2 (ellipse 20 30 "solid" "blue"))
(ellipse 40 60 "solid" "blue")
(scale/xy  3
      2
      (ellipse 20 30 "solid" "blue"))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAME AND CROP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
"frame and crop examples from DrRacket documentation"
(frame (ellipse 20 20 "outline" "black"))

(beside
     (ellipse 20 70 "solid" "lightsteelblue")
     (frame (ellipse 20 50 "solid" "mediumslateblue"))
     (ellipse 20 30 "solid" "slateblue")
     (ellipse 20 10 "solid" "navy"))
   
(crop 0 0 40 40 (circle 40 "solid" "chocolate"))
(crop 40 60 40 60 (ellipse 80 120 "solid" "dodgerblue"))
(above
     (beside (crop 40 40 40 40 (circle 40 "solid" "palevioletred"))
             (crop 0 40 40 40 (circle 40 "solid" "lightcoral")))
     (beside (crop 40 0 40 40 (circle 40 "solid" "lightcoral"))
             (crop 0 0 40 40 (circle 40 "solid" "palevioletred"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINE, ADD-LINE & SCENE+LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"Three tests for line"
(line 30 30 "black")
  
(line -30 20 "red")
  
(line 30 -20 "red")

"Three tests for add-line"
(add-line (ellipse 40 40 "outline" "maroon")
              0 40 40 0 "maroon")
  
(add-line (rectangle 40 40 "solid" "gray")
              -10 50 50 -10 "maroon")
  
(add-line
      (rectangle 100 100 "solid" "darkolivegreen")
     25 25 100 100
      "goldenrod")
    
"Three tests for lines+scene: should be identical to above, but cropped around base image"
(scene+line (ellipse 40 40 "outline" "maroon")
                0 40 40 0 "maroon")
  
(scene+line (rectangle 40 40 "solid" "gray")
                -10 50 50 -10 "maroon")
  
(scene+line
     (rectangle 100 100 "solid" "darkolivegreen")
     25 25 100 100
      "goldenrod")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLIP-VERTICAL & FLIP-HORIZONTAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"a red triangle, a blue one flippled horizontally and a green one flippled vertically"
(right-triangle 30 40 "solid" "red")
(flip-horizontal (right-triangle 30 40 "solid" "blue"))
(flip-vertical (right-triangle 30 40 "solid" "green"))

"those three triangles beside each other"
(beside (right-triangle 30 40 "solid" "red")
  (flip-horizontal (right-triangle 30 40 "solid" "blue"))
  (flip-vertical (right-triangle 30 40 "solid" "green")))



"one image flipped vertically, and one flipped horizontally"
(flip-vertical (image-url "http://racket-lang.org/logo.png"))
(flip-horizontal (image-url "http://racket-lang.org/logo.png"))

"BESIDE: reference image"
(beside (square 20 "solid" (make-color  50  50 255))
             (square 34 "solid" (make-color 150 150 255)))

"flip the second one horizontally"          
(beside (square 20 "solid" (make-color  50  50 255))
             (flip-horizontal (square 34 "solid" (make-color 150 150 255))))

"flip the second one vertically"          
(beside (square 20 "solid" (make-color  50  50 255))
             (flip-vertical (square 34 "solid" (make-color 150 150 255))))
          
"flip the first one horizontally"          
(beside (flip-horizontal (square 20 "solid" (make-color  50  50 255)))
             (square 34 "solid" (make-color 150 150 255)))

"flip the first one vertically"          
(beside (flip-vertical (square 20 "solid" (make-color  50  50 255)))
             (square 34 "solid" (make-color 150 150 255)))
          
"ABOVE: reference image"
(above (square 20 "solid" (make-color  50  50 255))
             (square 34 "solid" (make-color 150 150 255)))

"flip the second one horizontally"          
(above (square 20 "solid" (make-color  50  50 255))
             (flip-horizontal (square 34 "solid" (make-color 150 150 255))))

"flip the second one vertically"          
(above (square 20 "solid" (make-color  50  50 255))
             (flip-vertical (square 34 "solid" (make-color 150 150 255))))
          
"flip the first one horizontally"          
(above (flip-horizontal (square 20 "solid" (make-color  50  50 255)))
             (square 34 "solid" (make-color 150 150 255)))

"flip the first one vertically"          
(above (flip-vertical (square 20 "solid" (make-color  50  50 255)))
             (square 34 "solid" (make-color 150 150 255)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE PROPERTIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"(image-width (ellipse 30 40 'solid' 'orange'))"
(image-width (ellipse 30 40 "solid" "orange"))

"(image-width (circle 30 'solid' 'orange'))"
(image-width (circle 30 "solid" "orange"))

"(image-width (beside (circle 20 'solid' 'orange') (circle 20 'solid' 'purple')))"
(image-width (beside (circle 20 "solid" "orange") (circle 20 "solid" "purple")))
             
"(image-height (overlay (circle 20 'solid' 'orange') (circle 30 'solid' 'purple')))"
(image-height (overlay (circle 20 "solid" "orange") (circle 30 "solid" "purple")))

"(image-height (rectangle 10 0 'solid' 'purple'))"
(image-height (rectangle 10 0 "solid" "purple"))

"(image-baseline (text 'Hello' 24 'black'))"
(image-baseline (text "Hello" 24 "black"))

"(image-baseline (text/font 'Goodbye' 48 'indigo' 'Helvetica' 'modern 'normal 'normal #f))"
(image-baseline (text/font "Goodbye" 48 "indigo" "Helvetica" 'modern 'normal 'normal #f))


"(image-height (text/font 'Goodbye' 48 'indigo' 'Helvetica' 'modern 'normal 'normal #f))"
(image-height (text/font "Goodbye" 48 "indigo" "Helvetica" 'modern 'normal 'normal #f))
            
"(image-baseline (rectangle 100 100 'solid' 'black'))"
(image-baseline (rectangle 100 100 "solid" "black"))

"(image-height (rectangle 100 100 'solid' 'black'))"
(image-height (rectangle 100 100 "solid" "black"))


"(mode? 'outline')"
(mode? "outline")

"(mode? 'checkered')"
(mode? "checkered")

"(image-color? 'pink')"
(image-color? "pink")

"(image-color? 'puke')"
(image-color? "puke")

"(y-place? 'middle')"
(y-place? "middle")

"(x-place? 'up-top')"
(x-place? "up-top")

"(angle? 290)"
(angle? 290)

"(angle? -290)"
(angle? -290)

"(side-count? 20)"
(side-count? 20)

"(side-count? 2)"
(side-count? 2)

"(step-count? 2)"
(step-count? 2)

"(step-count? 0)"
(step-count? 0)
