;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname spacewar-abstraction) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Fire-fighting - Abstraction

; FIRE-FIGHTING

; PHYSICAL CONSTRAINTS

(define UNIT 10)

(define WIDTH 20)

(define HEIGHT 40)

; GRAPHICAL CONSTRAINTS

(define BG
  (empty-scene
   (* WIDTH UNIT) (* HEIGHT UNIT)))

(define FIRE
  (circle UNIT "solid" "red"))

(define WATER
  (circle UNIT "solid" "blue"))
(define PLANE
  (overlay/align
   "middle" "bottom"
   (ellipse (* 1 UNIT) (* 0.3 UNIT) "solid" "black")
   (overlay/align
    "middle" "middle"
    (ellipse (* 2.3 UNIT) (* 0.6 UNIT) "solid" "black")
    (ellipse (* 0.6 UNIT) (* 1.7 UNIT) "solid" "black"))))

; DATA DEFINITION =============================================

(define-struct ff [fires waters plane])
; (make-ff List-of-Posns List-of-Posns Posn)
; interpretation (make-ff f w p)
; f is the location of fires
; w is the location of waters
; p is the position of the plane

(define fires0 (list (make-posn 4 5) (make-posn 5 9)))
(define waters0 (list (make-posn 5 6) (make-posn 3 5)))
(define plane0 (make-posn 3 5))

(define ff0 (make-ff
             (list)
             (list)
             (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))))

(define ff1 (make-ff
             (list (make-posn 4 5) (make-posn 5 9))
             (list (make-posn 5 6) (make-posn 3 5))
             (make-posn 3 5)))

; FUNCTIONS =============================================

; RENDER
; FF -> Image
; Make image from a ff state

(check-expect
 (render
  (make-ff
   (list (make-posn 4 5) (make-posn 5 9))
   (list (make-posn 5 6) (make-posn 3 5))
   (make-posn 3 5)))
 (place-image
  PLANE
  (physical-location 3) (physical-location 5)
  (place-waters
   (list (make-posn 5 6) (make-posn 3 5))
   (place-fires
    (list (make-posn 4 5) (make-posn 5 9))
    BG))))
    

(define (render f)
  (place-image
   PLANE
   (physical-location (posn-x (ff-plane f)))
   (physical-location (posn-y (ff-plane f)))
   (place-waters
    (ff-waters f)
    (place-fires
     (ff-fires f)
     BG))))

; PHYSICAL-LOCATION
; Number -> Number
; convert a logical location to a physical location

(check-expect
 (physical-location 5)
 (* 5 UNIT))

(define (physical-location n)
  (* n UNIT))

; PLACE-WATERS
; List-of-posns Image -> Image
; Take a list of water positions and display them

(check-expect
 (place-waters
  (list (make-posn 5 6) (make-posn 3 5)) BG)
 (place-image
  WATER
  (physical-location 5) (physical-location 6)
  (place-image
   WATER
   (physical-location 3) (physical-location 5)
   BG)))

(check-expect (place-waters (list) BG) BG)
 

(define (place-waters alop scene)
  (abst-place-images WATER alop scene))



; PLACE-FIRES
; List-of-posns Image-> Image
; Take a list of fire positions and display them

(check-expect
 (place-fires
  (list (make-posn 5 6) (make-posn 3 5)) BG)
 (place-image
  FIRE
  (physical-location 5) (physical-location 6)
  (place-image
   FIRE
   (physical-location 3) (physical-location 5)
   BG)))

(check-expect (place-fires (list) BG) BG)

(define (place-fires alop scene)
  (abst-place-images FIRE alop scene))

; ABST-PLACE-IMAGES
; Image List-of-posns Image -> Image
(define (abst-place-images ele alop scene)
  (local (; Posn Image -> Image
          ; add ele to sce
          (define (add-ele p sce)
            (place-image
             ele
             (physical-location (posn-x p))
             (physical-location (posn-y p))
             sce)))
    (foldr add-ele scene alop)))


; TOCK
; FF -> FF
; Change FF state according to time

(check-expect
 (tock (make-ff
        (list)
        (list)
        (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1)))
       (make-posn 4 5))
 (make-ff
  (list (make-posn 4 5))
  (list)
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 2))))


(check-expect
 (tock (make-ff
        (list)
        (list)
        (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1)))
       (make-posn 5 6))
 (make-ff
  (list (make-posn 5 6))
  (list)
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 2))))

(check-expect
 (tock (make-ff
        (list
         (make-posn 3 4)
         (make-posn 4 5)
         (make-posn 4 4))
        (list)
        (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1)))
       (make-posn 5 6))
 (make-ff
  (list
   (make-posn 3 4)
   (make-posn 4 5)
   (make-posn 4 4))
  (list)
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 2))))

(define (tock f newfire)
  (make-ff
   (if
    (>= (length (ff-fires f)) 3)
    (ff-fires f)
    (cons newfire (ff-fires f)))
   (ff-waters f)
   (make-posn
    (posn-x (ff-plane f))
    (sub1 (posn-y (ff-plane f))))
   ))

; RAND-TOCK
; FF -> FF
; give random fires to tock

(check-random
 (rand-tock (make-ff
             (list)
             (list)
             (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))))
 (tock
  (make-ff
   (list)
   (list)
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1)))
  (make-posn (random WIDTH) (random HEIGHT))))

(define (rand-tock f)
  (tock f (make-posn (random WIDTH) (random HEIGHT))))
  

; KEY
; FF Key -> FF
; Change FF according to key input
; - " " releases water
; - "left" moves plane left
; - "right" moves plane right

(check-expect
 (key
  (make-ff
   (list)
   (list)
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))) "left")
 (make-ff
  (list)
  (list)
  (make-posn (sub1 (floor (/ WIDTH 2))) (- HEIGHT 1))))

(check-expect
 (key
  (make-ff
   (list)
   (list)
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))) "right")
 (make-ff
  (list)
  (list)
  (make-posn (add1 (floor (/ WIDTH 2))) (- HEIGHT 1))))

(check-expect
 (key
  (make-ff
   (list)
   (list)
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))) " ")
 (make-ff
  (list)
  (list (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1)))
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))))

(check-expect
 (key
  (make-ff
   (list)
   (list (make-posn 3 5)
         (make-posn 4 3)
         (make-posn 4 6)
         (make-posn 3 4)
         (make-posn 2 4)
         (make-posn 3 4)
         (make-posn 2 4))
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))) " ")
 (make-ff
  (list)
  (list (make-posn 3 5)
        (make-posn 4 3)
        (make-posn 4 6)
        (make-posn 3 4)
        (make-posn 2 4)
        (make-posn 3 4)
        (make-posn 2 4))
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))))

(check-expect
 (key
  (make-ff
   (list)
   (list)
   (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))) "else")
 (make-ff
  (list)
  (list)
  (make-posn (floor (/ WIDTH 2)) (- HEIGHT 1))))

(define (key f ke)
  (cond
    [(or
      (string=? ke "left")
      (string=? ke "right"))
     (make-ff
      (ff-fires f)
      (ff-waters f)
      (move-plane (ff-plane f) ke))]
    [(and
      (string=? ke " ")
      (< (length (ff-waters f)) 7)
      )
     (make-ff
      (remove-fire (ff-plane f) (ff-fires f))
      (cons
       (ff-plane f)
       (ff-waters f))
      (ff-plane f))]
    [else f]))

; REMOVE-FIRE
; Posn List-of-Posns -> List-of-Posns
; remove f from alop if it exists

(check-expect
 (remove-fire
  (make-posn 3 4)
  (list (make-posn 2 3) (make-posn 3 5)))
 (list
  (make-posn 2 3)
  (make-posn 3 5)))

(check-expect
 (remove-fire
  (make-posn 3 4)
  (list (make-posn 3 4) (make-posn 3 5)))
 (list
  (make-posn 3 5)))

(define (remove-fire f alop)
  (local (;Posn -> Boolean
          ; check if f and p are not overlapping
          (define (not-overlap? p)
            (not (equal? f p))))
    (filter not-overlap? alop)))


; END
(define (end ff)
  (overlay
   (text "Game Over" 20 "black")
   BG))

; END?
; FF -> Boolean
; Check if the ff state is still valid

(check-expect
 (end?
  (make-ff
   (list (make-posn 4 5) (make-posn 5 9))
   (list (make-posn 5 6) (make-posn 3 5))
   (make-posn 0 -1)))
 #true)

(check-expect
 (end?
  (make-ff
   (list (make-posn 4 5) (make-posn 5 9))
   (list (make-posn 5 6) (make-posn 3 5))
   (make-posn 0 1)))
 #false)

(define (end? f)
  (local ((define movedplane (move-plane (ff-plane f) "up")))
    (< (posn-y movedplane) -1)))


; MOVE-PLANE
; Posn KeyEvent -> Posn
; move the plane by a certain direction

(check-expect
 (move-plane (make-posn 3 5) "up")
 (make-posn 3 4))

(check-expect
 (move-plane (make-posn 3 5) "left")
 (make-posn 2 5))

(check-expect
 (move-plane (make-posn 3 5) "right")
 (make-posn 4 5))

(check-expect
 (move-plane (make-posn 3 5) "else")
 (make-posn 3 5))

(define (move-plane p ke)
  (cond
    [(string=? "up" ke)
     (make-posn
      (posn-x p)
      (sub1 (posn-y p)))]
    [(string=? "left" ke)
     (make-posn
      (sub1 (posn-x p))
      (posn-y p))]
    [(string=? "right" ke)
     (make-posn
      (add1 (posn-x p))
      (posn-y p))]
    [else p]))

; MAIN
; FF -> FF
; Start program from some initial state
(define (main f)
  (big-bang f
    [on-draw render]
    [stop-when end?]
    [on-key key]
    [on-tick rand-tock 0.2]
    ))

; (main ff0)




