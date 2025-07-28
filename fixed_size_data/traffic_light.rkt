;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |traffic light|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
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

;- Wishes
;render, tock


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
(traffic-prog "red")
