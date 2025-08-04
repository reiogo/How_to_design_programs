;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname WorldPrograms) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; the following constants descrive distances in terms of pixels
(define HEIGHT 60)
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))

; graphical constants
(define BACKGROUND
  (empty-scene WIDTH HEIGHT))
  ;(rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT
  (triangle 3 "solid" "red"))
  ;(rectangle 3 10 "solid" "red"))

; A List-of-shots is one of:
; - '()
; - (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number
; interpretation the number represents the shot's y-coordinate

; A ShotWorld is List-of-Numbers.
; interpretation each number represents the y-coordinate of a shot

; TO-IMAGE
; ShotWorld -> Image
; adds each y on w at (MID, y) to the background image

; given '() expect background
(check-expect (to-image '()) BACKGROUND)
; given list expect results
(check-expect (to-image (cons 10 '()))
              (place-image SHOT XSHOTS 10 BACKGROUND))
(check-expect
 (to-image (cons 50 (cons 10 '())))
 (place-image SHOT XSHOTS 50 
              (place-image SHOT XSHOTS 10 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image
           SHOT XSHOTS
           (first w) (to-image (rest w)))]))

; TOCK
; ShotWorld -> ShotWorld
; moves each shot up by one pixel

; given '() expect '()
(check-expect (tock '()) '())
; given a list expect the results
(check-expect
 (tock (cons 5 '())) (cons 4 '()))
(check-expect
 (tock (cons 59 (cons 5 '()))) (cons 58 (cons 4 '())))

(define (tock w)
  (cond
    [(empty? w) '()]
    [(cons? w)
     (cons (sub1 (first w)) (tock (rest w)))]))

; INCORREMOVE
; Number ShotWorld-> ShotWorld
; increment the number or remove it if it is out the canvas
(check-expect (incOrRemove 1 '()) (cons 0 '()))
(check-expect (incOrRemove -1 '()) '())
(define (incOrRemove f w)
   (if (< f 0) w (cons (sub1 f) w)))

; TOCK2
; ShotWorld -> ShotWorld

; given a shot that is outside the canvas, then it is erased
(check-expect (tock2 (cons -1 '())) '())
(check-expect (tock2 (cons 4 (cons -1 '()))) (cons 3 '()))
; given '() expect '()
(check-expect (tock2 '()) '())
; given a list expect the results
(check-expect
 (tock2 (cons 5 '())) (cons 4 '()))
(check-expect
 (tock2 (cons 59 (cons 5 '()))) (cons 58 (cons 4 '())))

(define (tock2 w)
  (cond
    [(empty? w) '()]
    [(cons? w) (incOrRemove (first w) (tock2 (rest w)))]))

; KEYH
; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world if the space bar was hit

(check-expect
 (keyh '() "d") '())
(check-expect
 (keyh '() " ") (cons HEIGHT '()))
(check-expect
 (keyh (cons 4 '()) " ") (cons HEIGHT (cons 4 '())))
(define (keyh w ke)
  (cond
    [(key=? ke " ")
     (cons HEIGHT w)]
    [else w]))


; ShotWorld -> ShotWorld
; runs the shot program from a given state
(define (main w0)
  (big-bang w0
    [on-draw to-image]
    [on-tick tock2]
    [on-key keyh]))
(main '())




     






