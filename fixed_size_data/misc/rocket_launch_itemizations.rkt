;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket_launch_itemizations) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A LRCD (short for launching rocket count down) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber
; interpretation a rocket resting on the ground, in count-down mode,
; or the number of pixels between the top and the rocket (its height)
; these constants express distances in terms of pixels

(define HEIGHT 300)
(define WIDTH 100)
(define YDELTA 3)
(define X-pos 10)

; graphical constants
(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define ROCKET-HEIGHT (image-height ROCKET))
(define ROCKET-CENTER (/ ROCKET-HEIGHT 2)) ; pixels


;LRCD->Image
;auxiliary function that puts the rocket on the background
; in the appropriate place
(check-expect
 (_rocket-place HEIGHT)
 (place-image ROCKET X-pos (- HEIGHT (+ HEIGHT ROCKET-CENTER)) BACKG))
(check-expect
 (_rocket-place 10)
 (place-image ROCKET X-pos (- HEIGHT (+ 10 ROCKET-CENTER)) BACKG))
(define (_rocket-place x)
  (place-image ROCKET X-pos (- HEIGHT (+ x ROCKET-CENTER)) BACKG))

;LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect
 (show "resting")
 (_rocket-place 0))
(check-expect
 (show 0)
 (_rocket-place 0))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              X-pos (* 3/4 WIDTH)
              (_rocket-place 0)))
(check-expect
 (show 53)
 (_rocket-place 53))

(define (show x)
  (cond
    [(string? x)
     (_rocket-place (- HEIGHT HEIGHT))]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  X-pos (* 3/4 WIDTH)
                  (_rocket-place 0))]
    [(>= x 0)
     (_rocket-place x)]))

; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed,
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x (- HEIGHT HEIGHT)) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

;LRCD -> Boolean
;Ends when the rocket is out of sight
(check-expect (end? "resting") #false)
(check-expect (end? HEIGHT) #true)
(check-expect (end? -2) #false)
(check-expect (end? 15) #false)
(define (end? x)
  (cond
    [(string? x) #false]
    [(<= -3 x -1) #false]
    [(= x HEIGHT) #true]
    [else #false]))

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [to-draw show]
    [on-key launch]))


; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-key launch]
    [on-tick fly]
    [stop-when end?]))
(main2 "resting")



