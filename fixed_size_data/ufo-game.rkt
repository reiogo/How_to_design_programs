;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo-game) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; constants
(define UNIT 1)
(define HEIGHT (* 300 UNIT))
(define WIDTH (* 200 UNIT))
(define BG (empty-scene WIDTH HEIGHT))
(define TANK-HEIGHT (* UNIT 15))

; graphical constants
(define tank-image
  (rectangle (* UNIT 10) (* UNIT 3) "solid" "black"))
(define UFO
  (overlay (circle 8 "solid" "green")
(rectangle 35 2 "solid" "green")))
(define missile
  (triangle 5 "solid" "blue"))
(define tree
(underlay/xy (circle 10 "solid" "green")
9 15
(rectangle 2 20 "solid" "brown")))

; initial scene
(define scene1
  (place-image
   tank-image
   15 (- HEIGHT 10)
   (place-image
    UFO
    25 15
    (place-image
     missile
     20 155
     (place-image
      tree
      (- WIDTH 30) (- HEIGHT 30)
      BG)))))


; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's current location
; (using the normal top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;  (make-tank Number Number).
; interpretation (make-tank x dx) means the tank is at position
; (x, HEIGHT) and that it moves dx pixels per clock tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missiles current location

; A SIGS is one of:
; -(make-aim UFO TANK)
; -(make-fired UFO Tank Missile)
; interpretation represents the state of the space invader game
(make-aim (make-posn 20 10) (make-tank 28 -3))
(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))
(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))

