;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bouncing-ball) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct ball [location velocity])
; A Ball-1d is a structure:
; (make-ball Number Number)
;interpretation 1 the position from top adn the velocity
;interpretation 2, the position from left and the velocity

; A Ball-2d is a structure:
; (make-ball Posn Vel)
; interpretation a 2-dimensional position with a 2-dimensional velocity

(define-struct vel [deltax deltay])
; A Vel is a structure:
; (make-vel Number Number)
; interpretation (make-vel dx dy) means a velocity of dx pixels[per tick]
; along the horixontal and dy pixels [per tick] along the vertical direction


(define b1
  (make-ball 10 -3))

(define SPEED 3)

(define-struct balld [location direction])
(define b2
  (make-balld 10 "down"))


(define ball1 (make-ball (make-posn 30 40) (make-vel -10 5)))

(define-struct ballf[x y deltax deltay])
(define ball2 (make-ballf 30 40 -10 5))

;phone numbers
(define-struct centry [name home officev cell])

(define-struct phone [area number])
; A Phone is a structure:
;(make-phone Number String)
;interpretation, area code and local code

(define-struct phone# [area switch num])
; A Phone# is a structure:
; (make-phone# number number number)
;interpretation, area code (000 to 999),
; switch number(000 to 999), neighborhood code(000 to 999).

(define sf (make-centry "Shriram Fisler"
             (make-phone 207 "363-2421")
             (make-phone 101 "776-1099")
             (make-phone 208 "112-9981")))


; A Posn is a structure:
;(make-posn Number Number)
; interpretation a location x pixels from left and y from the top
(define-struct posn [x y])

(define a-posn (make-posn 7 0))

(define-struct entry [name phone email])
; An Entry is a structure:
; (make-entry String String String)
; interpretation, a contact's name, 7 digit phone#, and email address
(define pl
(make-entry "Sarah Lee" "666-7771" "lee@classy-university.edu"))

; the following constants describe distances in terms of pixels
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH 400)
(define CENTER (quotient WIDTH 2))

(define-struct game [left-player right-player ball])
(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))

