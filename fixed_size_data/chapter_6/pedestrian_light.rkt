;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pedestrian_light) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; physical constraints
(define UNIT 1)
(define HEIGHT (* UNIT 100))
(define WIDTH (* UNIT 100))
(define BG (empty-scene WIDTH HEIGHT))

; graphical constraints
(define p-red
  (circle 25 "solid" "red"))
(define p-green
  (circle 25 "solid" "green"))

; A WorldState is one of
; - Light
; - Number between 0 and 9
; interpretation, the displayed screen in that moment.

(define-struct light [color time])
; (make-light String Number)
; interpretation, (make-light c t)
; c is the color of the light, t is the number of clock-ticks
; that have passed.

;functions: render, tock, key

; SIGN-IS
; Image -> Image
; Place image on BG
(check-expect (sign-is p-red)
              (overlay p-red BG))
(define (sign-is img)
  (overlay img BG))

; EVEN-NUM
; Number -> Image
; creates an image of a green number
(check-expect (even-num 4)
              (text (number->string 4) 25 "green"))
(define (even-num n)
  (text (number->string n) 25 "green"))

; ODD-NUM
; Number -> Image
; creates an image of a green number
(check-expect (odd-num 3)
              (text (number->string 3) 25 "red"))
(define (odd-num n)
  (text (number->string n) 25 "red"))

; RENDER - wish, sign-is, even-num, odd-num
; WorldState -> Image
; Interprets the worldstate as an image
; if red shows red light, if green shows green light,
; if odd number shows red number, if even shows green number
; given ws of green render a green light on an BG
(check-expect (render (make-light "green" 4))
              (sign-is p-green))
; so forth with red
(check-expect (render (make-light "red" 5))
              (sign-is p-red))
; so forth with number of the green (even)
(check-expect (render 6)
              (sign-is (even-num 6)))
; so forth with the number of red (odd)
(check-expect (render 7)
              (sign-is (odd-num 7)))
(define (render ws)
  (cond
    [(light? ws)
     (cond
       [(string=? (light-color ws) "red") (sign-is p-red)]
       [(string=? (light-color ws) "green") (sign-is p-green)])]
    [(number? ws)
     (cond [(= (modulo ws 2) 0)
            (sign-is (even-num ws))]
           [(>= (modulo ws 2) 1)
            (sign-is (odd-num ws))])]))

; TOCK
; WorldState -> WorldState
; Changes the world state to the next state.
; if red or green then stay the same, if number greater than 0 then -1.
; given red and the time is 10 then ws should be 9
(check-expect (tock (make-light "red" 10)) 9)
; given green and the time is 10 then ws should be red
(check-expect (tock (make-light "green" 10))
              (make-light "red" 0))
; given red or green expect no change
(check-expect (tock (make-light "green" 5)) (make-light "green" 6))
(check-expect (tock (make-light "red" 4)) (make-light "red" 5))
; given 0 expect green
(check-expect (tock 0) (make-light "green" 0))
; given any other number less than 10 expect decrement by 1
(check-expect (tock 4) 3)
(define (tock ws)
  (cond [(light? ws)
         (cond [(= (light-time ws) 10)
                (cond
                  [(string=? (light-color ws) "red") 9]
                  [(string=? (light-color ws) "green")
                   (make-light "red" 0)])]
               [(string=? "red" (light-color ws))
                (make-light
                 "red"
                 (+ (light-time ws) 1))]
               [(string=? "green" (light-color ws))
                (make-light
                 "green"
                 (+ (light-time ws) 1))])]
        [(number? ws)
         (cond [(= 0 ws) (make-light "green" 0)]
               [(< 0 ws 10) (- ws 1)])]))

; KEY
; WorldState KeyEvent -> WorldState
; On key input change the world state
; - on space bar change from green to red
; given "red" and " " expect "red"
(check-expect (key (make-light "red" 0) " ")
              (make-light "green" 0))
(check-expect (key (make-light "red" 0) "f")
              (make-light "red" 0))
(define (key ws ke)
  (cond [(string=? ke " ")
         (make-light "green" 0)]
        [else ws]))


; main program
; run the program with a certain initial state
(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key]
    [on-tick tock]
    ))

(main (make-light "red" 0))
;(main 3)







