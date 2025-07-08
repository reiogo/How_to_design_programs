;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname worldprograms) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;physical constants
(define WIDTH-OF-WORLD 200)
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

;graphical constants
(define tree
  (underlay/xy(circle 10 "solid" "green")
              9 15
              (rectangle 2 20 "solid" "brown")))
(define BACKGROUND
  (overlay/offset
   tree
   (- 0 (* WIDTH-OF-WORLD .2))  0
    (rectangle WIDTH-OF-WORLD
             (/ WIDTH-OF-WORLD 10)
              "solid" "white" )
     ))
(define Y-CAR 22)
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-RADIUS WHEEL-RADIUS
                         "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR-BODY 
  (above
   (rectangle (* WHEEL-RADIUS 2.5) (* WHEEL-RADIUS 1.5)
              "solid" "red")
    (rectangle
     (* WHEEL-DISTANCE 1.2)
     (* WHEEL-RADIUS 2) "solid" "red")))
(define CAR
  (overlay/offset BOTH-WHEELS 0 (- 0 (* WHEEL-RADIUS 2)) CAR-BODY))


;A WorldState is a Number.
;interpretation the number of pixels
;between the left border and the car

;clock-tick-handler
;WorldState -> WorldState
;moves the car by three pixels
;every time the clock ticks
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

;render
;WorldState -> Image
;place the car into a scene according to the given
;world state
(check-expect (render 5) (place-image CAR (- 5 (/ (image-width CAR) 2)) Y-CAR BACKGROUND))
(check-expect (render 40) (place-image CAR (- 40 (/ (image-width CAR) 2)) Y-CAR BACKGROUND))
(define (render ws)
  (place-image CAR (- ws (/ (image-width CAR) 2)) Y-CAR BACKGROUND))

;end?
;WorldState -> Boolean
;when the car leaves the scene, the scene ends
(check-expect (end? 0) #false)
(check-expect (end? 200) #true)
(define (end? ws)
  (cond
    [(>= ws WIDTH-OF-WORLD)
     #true]
    [(< ws WIDTH-OF-WORLD)
     #false]
    ))


;wish
;WorldState Number Number String -> WorldState
;places the car at the x-coordinate if me is "button-down"
;given 21 10 20 "enter"
;wanted 21
;given 42 10 20 "button-down"
;wanted 10;
;given 42 10 20 "move"
;wanted 42
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 21 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)

(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

;WorldState -> WorldState
;launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]
    [on-mouse hyper]))
(main 1)



;--------------------------------------------------


;An AnimationState is a Number
;interpretation the number of clock ticks since the animation started

;AnimationState -> Position
;convert the time into the position where the car will be placed
(check-expect (dist 4) 12)
(define (dist t)
  (* t 3))

;AnimationState -> AnimationState
;simply return how many seconds has passed
(check-expect (tock2 2) 3)
(define (tock2 n)
  (+ n 1))

;AnimationState -> Image
;place car according to distance from start
;(check-expect (render2 2) (place-image CAR 6 Y-CAR BACKGROUND))
(define (render2 d)
  (place-image CAR (dist d) Y-CAR BACKGROUND))

;AnimationState -> AnimationState
;launches program from some initial state
(define (main2 n)
  (big-bang n
    [on-tick tock2]
    [to-draw render2]
    [on-mouse hyper]
    ))




