#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world))


;; Constants:

(define E "empty") ;See CellValue data definition below
(define D "dot")   ;
(define W "wall")  ;

(define INIT-BOARD ;See Board data definition below
  (vector (vector W W W W W W W W W W W W W)
          (vector W D D D D D D D D D D D W)
          (vector W D W D W W W W W D W D W)
          (vector W D W D W D D D W D W D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W W D W W W D W W D W)
          (vector W D D D D D E D D D D D W)
          (vector W W W W W W W W W W W W W)))

(define SMALL-BOARD
  (vector (vector E E E)
          (vector E E E)))

(define CELL-SIZE 20)

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))

(define SMALL-BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref SMALL-BOARD 0))))
(define SMALL-BOARD-HEIGHT (* CELL-SIZE (vector-length SMALL-BOARD)))

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)

(define PM (circle 10 "solid" "yellow"))

(define MTC  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; empty cell
(define DTC  (overlay (circle 3  "solid" "white") MTC))       ; dot in cell
(define WALL (rectangle CELL-SIZE CELL-SIZE "solid" "blue"))  ; wall

(define MTB 
  (empty-scene BOARD-WIDTH
               (+ BOARD-HEIGHT SCORE-HEIGHT)))

(define SMALL-MTB
  (empty-scene SMALL-BOARD-WIDTH
               (+ SMALL-BOARD-HEIGHT SCORE-HEIGHT)))



;; Data definitions:


;; Score is Natural
;; interp. dots eaten by pac-man since start of game

(define INIT-SCORE  0)

;; CellValue is one of:
;; - "empty"
;; - "dot"
;; - "wall"
;; interp. the content of a board cell

;; Direction is one of:
;; - "U"
;; - "D"
;; - "L"
;; - "R"
;; interp. direction that a sprite is facing

(define-struct sprite (x y dir))
;; Sprite is (make-sprite Natural Natural Direction)
;; interp. the position in Board coordinates, and the direction of a sprite

(define INIT-PM (make-sprite 6 6 "U"))

;; Board is (vectorof (vectorof CellValue))
;; interp. the game board

(define RENDER-TEST-BOARD (vector (vector W E)
                                  (vector D E)))

(define-struct gs (pm board board-image score))
;; GameState is (make-gs Sprite Board Image Score)
;; interp. all parts of the pac-man game; pac-man, the current
;; board, the current board image, and the current score

(define MTB-GS (make-gs INIT-PM INIT-BOARD MTB INIT-SCORE))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Testing values:

;; Sprite:
(define R-SPRITE (make-sprite 1 1 "R"))
(define L-SPRITE (make-sprite 1 1 "L"))
(define U-SPRITE (make-sprite 1 1 "U"))
(define D-SPRITE (make-sprite 1 1 "D"))

;; Board:
(define EE-BOARD (vector (vector W W W W)
                         (vector W E E W)
                         (vector W W W W)))

(define ED-BOARD (vector (vector W W W W)
                         (vector W E D W)
                         (vector W W W W)))

(define DD-BOARD (vector (vector W W W W)
                         (vector W D D W)
                         (vector W W W W)))

;; GameState:
;; MTB-GS previously defined above
(define END-GS (make-gs R-SPRITE EE-BOARD SMALL-MTB 0))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Functions:


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-tick handler:


;; GameState -> GameState
;; advances the game

(define (tick gs)
  (local [(define pm          (gs-pm gs))
          (define board       (gs-board gs))
          (define board-image (gs-board-image gs))
          (define score       (gs-score gs))
          (define new-pm          (tick-pm pm board))
          (define new-board       (tick-board board new-pm))
          (define new-board-image (tick-board-image board board-image new-pm))
          (define new-score       (tick-score new-pm board score))]
    (make-gs new-pm
             new-board
             new-board-image
             new-score)))

;; Sprite Board -> Sprite
;; updates pac-man's position based on its direction
(define (tick-pm pm bd)
  (local [(define x   (sprite-x pm))
          (define y   (sprite-y pm))
          (define dir (sprite-dir pm))]
    (make-sprite (checked-move-x x y dir bd)
                 (checked-move-y x y dir bd)
                 dir)))

;; Natural Natural Direction Board -> Natural
;; moves x in direction dir, unless it runs into a wall on bd or dir is not in the x direction
;; ASSUMPTION: assumes x, y is at least one cell away from any edge of bd

(define (checked-move-x x y dir bd)
  (cond [(string=? "L" dir) (restrict-move (sub1 x) y x (sub1 x) bd)]
        [(string=? "R" dir) (restrict-move (add1 x) y x (add1 x) bd)]
        [else x]))

;; Natural Natural Direction Board -> Natural
;; moves y in direction dir, unless it runs into a wall on bd or dir is not in the y direction
;; ASSUMPTION: assumes x, y is at least one cell away from any edge of bd

(define (checked-move-y x y dir bd)
  (cond [(string=? "U" dir) (restrict-move x (sub1 y) y (sub1 y) bd)]
        [(string=? "D" dir) (restrict-move x (add1 y) y (add1 y) bd)]
        [else y]))

;; Natural Natural Natural Natural Board -> Natural
;; produces new-coord if bd does not contain a wall at check-x, check-y; otherwise produces old-coord

(define (restrict-move check-x check-y old-coord new-coord bd)
  (if (string=? (board-ref bd check-x check-y) "wall")
      old-coord
      new-coord))

;; Board Sprite -> Board
;; if cell at pacman's position is not empty, make a new board in which it is

(define (tick-board bd pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (string=? (board-ref bd x y) "empty")
        bd
        (new-board-w-empty-at x y bd))))

;; Number Number Board -> Board
;; produces a new board with an empty cell at x, y

(define (new-board-w-empty-at x0 y0 bd)
  (map-board (lambda (x y cv)
               (if (and (= x0 x) (= y0 y))
                   "empty"
                   cv))
             bd))

;; Board Image Sprite -> Image
;; updates the board image with an empty cell at x, y if pac-man is in a cell with a dot
(define (tick-board-image bd board-image pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (string=? (board-ref bd x y) "dot")
        (place-cell-image MTC x y board-image)
        board-image)))

;; Sprite Board Score -> Score
;; increases by 1 the score if pac-man is now in a cell containing a dot
(define (tick-score new-pm last-board score)
  (local [(define x (sprite-x new-pm))
          (define y (sprite-y new-pm))]
    (cond [(string=? (board-ref last-board x y) "dot")
           (add1 score)]
          [else score])))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-key handler:


;; GameState KeyEvent -> GameState
;; updates pac-man's direction based on key
(define (key-handler gs key)
  (make-gs (new-dir-pm (gs-pm gs) key)
           (gs-board gs)
           (gs-board-image gs)
           (gs-score gs)))

;; Sprite KeyEvent -> Sprite
;; produces pac-man facing in a new direction based on key
(define (new-dir-pm pm key)
  (cond [(key=? "up"    key) (make-sprite (sprite-x pm) (sprite-y pm) "U")]
        [(key=? "down"  key) (make-sprite (sprite-x pm) (sprite-y pm) "D")]      
        [(key=? "left"  key) (make-sprite (sprite-x pm) (sprite-y pm) "L")]
        [(key=? "right" key) (make-sprite (sprite-x pm) (sprite-y pm) "R")]
        [else pm]))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; on-tilt handler:


;; ;; GameState Number Number Number -> GameState
;; ;; change pac-man's direction based on tilt
;; (define (tilt-handler gs yaw pitch roll)
;;   (make-gs (tilt-pm (gs-pm gs) pitch roll)
;;            (gs-board gs)
;;            (gs-board-image gs)
;;            (gs-score gs)))

;; ;; Sprite Number Number -> Sprite
;; ;; changes pac-man's Direction based on pitch and roll
;; (define (tilt-pm pm pitch roll)
;;   (make-sprite (sprite-x pm)
;;                (sprite-y pm)
;;                (tilt->dir (sprite-dir pm) pitch roll)))

;; ;; Direction Number Number -> Direction
;; ;; changes Direction if there is a prominant tilt, otherwise produces old dir
;; (define (tilt->dir dir pitch roll)
;;   (cond [(> (abs pitch) (abs roll))
;;          (if (positive? pitch)
;;              "U"
;;              "D")]
;;         [(> (abs roll) (abs pitch))
;;          (if (positive? roll)
;;              "R"
;;              "L")]
;;         [else dir]))

;; (define (key-handler gs a-key)
;;   (make-gs (key-pm pm a-key)
;;            (gs-board gs)
;;            (gs-board-image gs)
;;            (gs-source gs)))

;; (define (key-pm pm a-key)
;;   (make-sprite (sprite-x pm)
;;                (sprite-y pm)
;;                (cond
;;                 [(key=? a-key "left")
;;                  "L"]
;;                 [(key=? a-key "right")
;;                  "R"]
;;                 [(key=? a-key "up")
;;                  "U"]
;;                 [(key=? a-key "down")
;;                  "D"]
;;                 [else
;;                  (sprite-dir pm)])))





;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; stop-when handler:


;; GameState -> Boolean
;; determines if pac-man has eaten all the dots
(define (game-over? gs)
  (empty-board? (gs-board gs)))

;; Board -> Boolean
;; determines if the board is empty
(define (empty-board? bd)
  (foldr-board (lambda (x y cv b)
                 (and b (not (string=? cv "dot"))))
               true
               bd))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; to-draw handler:


;; GameState -> Image
;; draws the game
(define (render gs)
  (render-pm (gs-pm gs)
             (render-score (gs-score gs)
                           (gs-board-image gs))))

;; Board -> Image
;; draws the board
(define (render-board bd)
  (foldr-board (lambda (x y cv b)
                 (place-cell-image (cell-image cv) x y b))
               MTB
               bd))

;; Sprite Image -> Image
;; adds pac-man image to img
(define (render-pm pm img)
  (place-cell-image PM (sprite-x pm) (sprite-y pm) img))

;; Score Image -> Image
;; adds score to img
(define (render-score score img) 
  (local [(define score-text
            (text (string-append "Score: " (number->string score)) SCORE-TEXT-SIZE "black"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 BOARD-HEIGHT
                 img)))

;; CellValue -> Image
;; draws a board cell
(define (cell-image cv)
  (cond [(string=? cv "empty") MTC] 
        [(string=? cv "dot")   DTC]
        [(string=? cv "wall")  WALL]))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Operations on Board and other helpers:


;; Board Natural Natural -> CellValue
;; looks up the value of a Board cell
(define (board-ref bd x y)
  (vector-ref (vector-ref bd y) x))


(define (build-vector n f)
  (let ([vec (make-vector n)])
    (let loop ([i 0])
      (cond
       [(< i n)
        (vector-set! vec i (f i))
        (loop (add1 i))]))
    vec))
         

;; (Natural Natural CellValue -> CellValue) Board -> Board
;; the analogue of map for boards, the function is called for
;; each position in the board to produce a cell value for that
;; position in a new resulting board
(define (map-board fn bd)
  (build-vector (vector-length bd)
                (lambda (y)
                  (build-vector (vector-length (vector-ref bd y))
                                (lambda (x)
                                  (fn x y (board-ref bd x y)))))))

;; (Natural Natural CellValue X -> X) X Board -> X
;; the analogue of foldr for boards, the function is called for
;; each position in the board to produce single value
(define (foldr-board fn base bd)
  (local [(define nrows (vector-length bd))
          (define ncols (vector-length (vector-ref bd 0)))
          
          (define (rows y b)
            (cond [(= y nrows) b]
                  [else
                   (rows (add1 y)
                         (cols 0 y b))]))
          (define (cols x y b)
            (cond [(= x ncols) b]
                  [else
                   (cols (add1 x)
                         y
                         (fn x y (board-ref bd x y) b))]))]
    (rows 0 base)))

;; Image Natural Natural Image -> Image
;; adds cell-img to board-image at x, y board coordinates
(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))





;; -> GameState
;; runs the game
(local [(define INIT-GS (make-gs INIT-PM
                                 INIT-BOARD
                                 (render-board INIT-BOARD)
                                 INIT-SCORE))]
  (big-bang INIT-GS
            (on-tick tick 0.3)
            (to-draw render)
            (on-key key-handler)
            ;;(on-tilt tilt-handler)
            (stop-when game-over?)))
