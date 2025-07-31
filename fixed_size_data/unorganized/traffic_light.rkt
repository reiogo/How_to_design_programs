;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic_light) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;Design a program that changes traffic lights every sec

;- Constants
;physical
(define HEIGHT-OF-WORLD 100)
(define RADIUS (/ HEIGHT-OF-WORLD 2))
(define X (/ HEIGHT-OF-WORLD 2))
;graphical

;- Data representation
;WorldState is a String
;interpretation, the string represents the color

; Any -> Boolean
; is the given value an element of TrafficLight

;- Wishes
;render, tock

; TrafficLight TrafficLight -> Boolean
; are the two (states of) traffic lights equal

;(define (light=? a-value another-value)
 ; (string=? a-value another-value))

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

; Any Any -> Boolean
; are the two values elements of TrafficLight and,
; if so, are they equal
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
(check-error (light=? 4 "yellow")
              "traffic light expected for the first value, given: some other value")
(check-error (light=? "green" 3)
              "traffic light expected for the second value, given: some other value")

(define (light=? a-value another-value)
  (cond
    [(not (light? a-value))
     (error "traffic light expected for the first value, given: some other value")]
    [(not (light? another-value))
     (error "traffic light expected for the second value, given: some other value")]
    [(and (light? a-value) (light? another-value))
      (string=? a-value another-value)]))

;WorldState -> Image
;Convert a string into corresponding color traffic light
(check-expect (render "red")
              (place-image
               (circle RADIUS "solid" "red")
               X X
               (rectangle
                HEIGHT-OF-WORLD
                HEIGHT-OF-WORLD
                "solid" "white"))
              )
(define (render ws)
  (place-image
   (circle RADIUS "solid" ws)
   X X
   (rectangle
    HEIGHT-OF-WORLD
    HEIGHT-OF-WORLD
    "solid" "white")))


;WorldState -> WorldState
;Change the light to the next color
(check-expect (tock "red") "green")
(check-expect (tock "green") "yellow")
(check-expect (tock "yellow") "red")

(define (tock ws)
  (cond
    [(string=? "red" ws) "green"]
    [(string=? "green" ws) "yellow"]
    [(string=? "yellow" ws) "red"]
    ))
;- Main
;WorldState -> WorldState
;Traffic light that changes every second
(define (traffic-prog ws)
  (big-bang ws
    [on-tick tock]
    [on-draw render]))
;(traffic-prog "red")
