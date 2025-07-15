;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nested-structures) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct vel [deltax deltay])
; A Vel is a structure:
; (make-vel Number Number)
; interpretation (make-vel dx dy) means a velocity of dx pixels per [tick]
; along the horizontal axis, and dy pixels per [tick] along the vertical axis

(define-struct ufo [loc vel])
; A UFO is a structure:
; (make-ufo Posn Vel)
; interpretation (make-ufo p v) is at location p moving at velocity v
; for Vel, see above.
(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))

(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

;UFO -> UFO
;determines where u moves in one clock tick
;leaves the velocity as is

(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2) (make-ufo(make-posn 17 77) v2))

(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u)) (ufo-vel u)))

; Posn Vel -> Posn
; adds v to p
(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))
(define (posn+ p v)
  (make-posn
    (+ (posn-x p) (vel-deltax v))
    (+ (posn-y p) (vel-deltay v))))


(define-struct ball [location velocity])
(make-posn (make-ball "hello" 1) #false)
(make-posn (make-ball (make-ball (make-posn 1 2) 3) 4) 5)

(define-struct movie [title producer year])
; A Movie is a structure:
; (make-movie String String Number)
;interpretation, (make-movie t p y) t is the name of the movie,
; p is the producer of the movie
; y is the year the movie was released

(define-struct personi [name hair eyes phone])
; A Person is a structure:
; (make-person String String String String)
; interpretation, (make-person n h e p) n is the person's name
; h is the person's hair color
; e is the person's eye color
; p is the phone number

(define-struct pet [name number])
; A Pet is a structure:
; (make-pet String Number)
; interpretation, (make-pet n num) n is the name of the pet
; num is the id of the pet

(define-struct CD [artist title price])
; A CD is a structure
; (make-CD String String String)
; interpretation, (make-pet a t p) a is the artists name
; t is the title of the CD
; p is the price of the CD

(define-struct sweater [material size producer])
; A Sweater is a structure
; (make-sweater String String String)
; interpretation, (make-sweater m s p) m is the material name
; s is one of S, M, L
; p is the name of the person who made it

(define-struct time-from-midnight [hours minutes second])
; A Time-from-midnight is a structure
; (make-time-from-midnight Number Number Number)
; interpretation, (make-time-from-midnight h m s) represents a point in time from midnight where:
; h a number that describes how many hours its been since midnight (0 - 23)
; m a number that describes how many minutes it is past the hour (0 - 59).
; s a number that describes how many seconds it is past the minute (0 - 59).


(define-struct lc-3-letter-w [one two three])
; A Lc-3-letter-w is a structure:
; (make-lc-3-letter-w letterOrFalse letterOrFalse letterOrFalse)
; interpretation: represents a three letter word where each position
; [one, two, three] can be a single lowercase letter or a #false.
; #false indicates that the letter in the location is unknown or yet to be guessed.

; A letterOrFalse is one of:
; -String (a single lowercase letter from a-z)
; -Boolean (#false)

; A Color is one of:
; —"white"
; —"yellow"
; —"orange"
; —"green"
; —"red"
; —"blue"
; —"black"
; example:
(define WHITE "white")
(define GREEN "green")


; H is a Number between 0 and 100.
; interpretation represents a “happiness value”
; example:
(define LOWEST 0)
(define HIGHEST 100)
(define IN-BETWEEN 68)


(define-struct person [fstname lstname is-male])
; A Person is a structure:
; (make-person String String Boolean)
(make-person "Jon" "Jack" #true)
(make-person "Jona" "Jacky" #false)

(define-struct dog [owner name age happiness])
; A Dog is a structure:
; (make-dog Person String PositiveInteger H)
; interpretation, (make-dog o n a h) represents a dog.
; o is a Person who owns the dog
; n is the name of the dog
; a is the age in years (positive integer)
; h is the happiness level[0-100] where 0 is unhappy and 100 is happiest.f
(make-dog (make-person "Jacky" "John" #false) "Melbourne" 10 97)

; A Weapon is one of:
; —#false
; — Posn
; interpretation #false means the missile hasn't been fired yet;
; an instance of Posn means the missile is in flight
(define w1 #false)
(define w2 (make-posn 5 10))


