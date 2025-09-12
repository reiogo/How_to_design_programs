;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Tetris_iter1) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; Tetris

; PHYSICAL CONSTANTS
(define WIDTH 10) ; the maximal number of blocks horizontally
(define HEIGHT 20) ; the max num of blocks vertically


; GRAPHICAL CONSTANTS
(define SIZE 10) ; blocks are square
(define BLOCK ; they are rendered as red squares with black rims
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))
(define SCENE-SIZE (* WIDTH SIZE))

; physical-location
; N -> N
; convert pos to physical location

(check-expect
 (physical-location 4)
 (+ (* SIZE 4) (/ SIZE 2)))
 
(define (physical-location n)
  (+ (* SIZE n) (/ SIZE 2)))

(define BG
  (empty-scene (* SIZE WIDTH) (* SIZE HEIGHT)))

; DATA DEFINITION
  
(define-struct tetris (block landscape))
(define-struct block (x y))

; A Tetris is a structure:
; (make-tetris Block Landscape)

; A Landscape is one of:
; - '()
; - (cons Block Landscape)

; Block is a structure:
; (make-block N N)

; interpretation (make-block x y) depicts a block whose left
; corner is (* x SIZE) pixels from the left and (* y SIZE)
; pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the currently
; dropping block, while b1, b2, etc are the resting ones

(define landscape0 '())

(define block-dropping0 (make-block 4 5))

(define tetris0
  (make-tetris
   (make-block 4 0)
   landscape0))

(define tetris0-drop
  (make-tetris
   block-dropping0
   landscape0))

(define landscape1
  (list (make-block 6 (- HEIGHT 1))))

(define landscape2
  (list
   (make-block 4 (- HEIGHT 1))
   (make-block 4 (- HEIGHT 2))))

(define block-dropping1 (make-block 6 5))

(define tetris1
  (make-tetris
   (make-block 6 0)
   landscape1))

(define tetris1-drop
  (make-tetris
   block-dropping1
   landscape1))

(define tetris2
  (make-tetris
   (make-block 5 0)
   landscape1))

(define tetris3
  (make-tetris
   (make-block 5 0)
   (list
    (make-block 6 (- HEIGHT 1))
    )))

(define block-landed (make-block 0 (- HEIGHT 1)))

(define block-on-block (make-block 0 (- HEIGHT 2 )))

; FUNCTIONS

; TETRIS-RENDER
; Tetris -> Image
; Turn a tetris definition into an image

(check-expect
 (tetris-render tetris0)
 (place-image
  BLOCK
  (physical-location 4) (physical-location 0)
  BG))

(check-expect
 (tetris-render tetris1)
 (place-image
  BLOCK
  (physical-location 6) (physical-location 0)
  (place-image
   BLOCK
   (physical-location 6) (physical-location (- HEIGHT 1))
   BG)))

(define (tetris-render t)
  (place-image
   BLOCK 
   (physical-location (block-x (tetris-block t)))
   (physical-location (block-y (tetris-block t)))
   (landscape-render (tetris-landscape t))))

; LANDSCAPE-RENDER
; Landscape -> Image
; render l as an image

(check-expect
 (landscape-render landscape1)
 (place-image
  BLOCK
  (physical-location 6) (physical-location (- HEIGHT 1))
  BG))

(check-expect
 (landscape-render landscape2)
 (place-image
  BLOCK
  (physical-location 4) (physical-location (- HEIGHT 1))
  (place-image
   BLOCK
   (physical-location 4) (physical-location (- HEIGHT 2))
   BG)))

(define (landscape-render l)
  (cond
    [(empty? l) BG]
    [else
     (place-image
      BLOCK
      (physical-location (block-x (first l)))
      (physical-location (block-y (first l)))
      (landscape-render (rest l)))]))

; TETRIS-TOCK
; Tetris Block-> Tetris
; move falling block according to time
; when it lands on the floor
; or on another block then it stays there
; when a new block is needed uses newblock

(check-expect
 (tetris-tock tetris1 (make-block 6 0))
 (make-tetris
  (make-block 6 1)
  landscape1))

; on block
(check-expect
 (tetris-tock
  (make-tetris
   (make-block 6 (- HEIGHT 3))
   (list (make-block 6 (- HEIGHT 2)))) (make-block 7 0))
 (make-tetris
  (make-block 7 0)
  (list
   (make-block 6 (- HEIGHT 3))
   (make-block 6 (- HEIGHT 2)))))

(check-expect
 (tetris-tock
  (make-tetris
   (make-block 5 (- HEIGHT 2))
   (list (make-block 6 (- HEIGHT 1)))) (make-block 6 0))
 (make-tetris
  (make-block 6 0)
  (list
   (make-block 5 (- HEIGHT 1))
   (make-block 6 (- HEIGHT 1)))))

(define (tetris-tock t newblock)
  (cond
    [(on-block?
      (move-block (tetris-block t) "down")
      (tetris-landscape t))
     (make-tetris
      newblock
      (cons
       (tetris-block t)
       (tetris-landscape t)))]
    [(on-floor? (move-block (tetris-block t) "down"))
     (make-tetris
      newblock
      (cons
       (move-block (tetris-block t) "down")
       (tetris-landscape t)))]
    [else
     (make-tetris
      (move-block (tetris-block t) "down")
      (tetris-landscape t))]))

; RAND-TETRIS-TOCK
; Tetris -> Tetris
; Tetris-tock with random new blocks when required

(check-random
 (rand-tetris-tock
  (make-tetris
   (make-block 5 (- HEIGHT 2))
   (list (make-block 6 (- HEIGHT 1)))))
 (tetris-tock
  (make-tetris
   (make-block 5 (- HEIGHT 2))
   (list (make-block 6 (- HEIGHT 1))))
  (make-block (random WIDTH) 0)))
               

(define (rand-tetris-tock t)
  (tetris-tock
   t
   (make-block (random WIDTH) 0)))

(define (test-tetris-tock t)
  (tetris-tock
   t
   (make-block 7 0)))

; MOVE-BLOCK
; Block -> Block
; move b according to direction d

(check-expect
 (move-block
  (make-block 4 5) "down")
 (make-block 4 6))

(check-expect
 (move-block
  (make-block 4 5) "right")
 (make-block 5 5))

(check-expect
 (move-block
  (make-block 4 5) "left")
 (make-block 3 5))

(check-expect
 (move-block
  (make-block 4 5) "else")
 (make-block 4 5))

(define (move-block b d)
  (cond
    [(string=? d "down")
     (make-block
      (block-x b)
      (add1 (block-y b)))]
    [(string=? d "right")
     (make-block
      (add1 (block-x b))
      (block-y b))]
    [(string=? d "left")
     (make-block
      (sub1 (block-x b))
      (block-y b))]
    [else b]))

; ON-FLOOR?
; Block -> Boolean
; check if the block is on the floor or not

(check-expect
 (on-floor? (make-block 4 5))
 #false)
(check-expect
 (on-floor?
  (make-block 4 (- HEIGHT 1)))
 #true)

(define (on-floor? b)
  (= (- HEIGHT 1)
     (block-y b)))

; ON-BLOCK?
; Tetris -> Boolean
; check if the block is on another block

(check-expect
 (on-block?
  (make-block 6 (- HEIGHT 2))
  (list (make-block 6 (- HEIGHT 2))))
 #true)

(check-expect
 (on-block?
  (make-block 6 (- HEIGHT 3))
  (list (make-block 6 (- HEIGHT 2))))
 #false)

(define (on-block? t l)
  (member? t l))

; VALID-BLOCK?
; Tetris -> Boolean
; check if the block is still valid or not
; false when block overlaps blocks in landscape
; or when block is on the floor

(check-expect
 (valid-block?
  (make-tetris
   (make-block 5 0)
   (list
    (make-block 6 (- HEIGHT 1))
    )))
 #true)

(check-expect
 (valid-block?
  (make-tetris
   (make-block 5 (- HEIGHT 2))
   (list
    (make-block 6 (- HEIGHT 1))
    )))
 #false)

(check-expect
 (valid-block?
  (make-tetris
   (make-block 6 (- HEIGHT 3))
   (list
    (make-block 6 (- HEIGHT 2))
    )))
 #false)

(define (valid-block? t)
  (cond
    [(= (- HEIGHT 1)
        (block-y (move-block (tetris-block t) "down")))
     #false]
    [(member?
      (move-block (tetris-block t) "down")
      (tetris-landscape t))
     #false]
    [else #true]
    ))

; TETRIS-KEY
; Tetris KeyEvent -> Tetris
; Change tetris according to keyevent
; - if possible "left" moves left
; - if possible "right moves right

(check-expect
 (tetris-key
  (make-tetris
   (make-block 6 7)
   (list
    (make-block 6 (- HEIGHT 2))))
  "left")
 (make-tetris
  (make-block 5 7)
  (list
   (make-block 6 (- HEIGHT 2)))))

(check-expect
 (tetris-key
  (make-tetris
   (make-block 6 7)
   (list
    (make-block 6 (- HEIGHT 2))))
  "right")
 (make-tetris
  (make-block 7 7)
  (list
   (make-block 6 (- HEIGHT 2)))))

(define (tetris-key t ke)
  (cond
    [(and (string=? ke "left") (can-move? t ke))
     (make-tetris
      (move-block (tetris-block t) ke)
      (tetris-landscape t))]
    [(and (string=? ke "right") (can-move? t ke))
     (make-tetris
      (move-block (tetris-block t) ke)
      (tetris-landscape t))]
    [else t]))


; CAN-MOVE
; Tetris KeyEvent -> Boolean
; check if movement k is valid

(check-expect
 (can-move?
  (make-tetris
   (make-block 7 (- HEIGHT 3))
   (list
    (make-block 6 (- HEIGHT 3))
    (make-block 6 (- HEIGHT 2))
    (make-block 6 (- HEIGHT 1))))
  "left")
 #false)

(check-expect
 (can-move?
  (make-tetris
   (make-block 7 (- HEIGHT 3))
   (list
    (make-block 6 (- HEIGHT 3))
    (make-block 6 (- HEIGHT 2))
    (make-block 6 (- HEIGHT 1))))
  "right")
 #true)

(check-expect
 (can-move?
  (make-tetris
   (make-block 5 (- HEIGHT 3))
   (list
    (make-block 6 (- HEIGHT 3))
    (make-block 6 (- HEIGHT 2))
    (make-block 6 (- HEIGHT 1))))
  "right")
 #false)

(define (can-move? t k)
  (cond
    [(member?
      (move-block (tetris-block t) k)
      (tetris-landscape t)) #false]
    [(not (<= 0
              (block-x (move-block (tetris-block t) k))
              (- WIDTH 1)))
     #false]
    [else #true]
    ))

; END?
; Tetris -> Boolean
; check when the program should end
(define (end? t)
  (<= (block-y (first (tetris-landscape t))) 0))
  


; TETRIS-MAIN
; Tetris Number -> Tetris
; Plays the tetris game from some initial state
; interpretation t is the tetris state
; s is the speed at which tock runs

(define (tetris-main t s)
  (big-bang t
    [on-draw tetris-render]
    [on-tick rand-tetris-tock s]
    [on-key tetris-key]
    [stop-when end?]
    ))

(tetris-main tetris1 0.04)





























