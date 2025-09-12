;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* 14 h))

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
; given '() expect '()
(check-expect (wage* '()) '())
; given (cons 28 '()) expect (cons 336 '())
(check-expect (wage* (cons 28 '())) (cons (wage 28) '()))
; given 40, 28 expect 480 336
(check-expect (wage* (cons 40 (cons 28 '())))
              (cons (wage 40) (cons (wage 28) '())))
(check-error (wage* (cons 100 (cons 28 '())))
             "over a 100h")
(define (wage* alon)
  (cond
    [(empty? alon) '()]
    [(<= 100 (first alon)) (error "over a 100h")]
    [else
     (cons (wage (first alon)) (wage* (rest alon)))]))

; A List-of-ftemp is one of:
; - '()
; - (cons Number List-of-ftemp)
; interpretation, a list of numbers that are in fahrenheit

; A List-of-ctemp is one of:
; - '()
; - (cons Number List-of-ctemp)
; interpretation, a list of numbers that are in celsius

; Number -> Number
; convert fahrenheit to celsius
(define (cFC n)
  (* (- n 32) 5/9))

; List-of-ftemp -> List-of-ctemp
; convert fahrenheit list to celsius list
(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 30 '())) (cons (cFC 30) '()))
(define (convertFC alof)
  (cond
    [(empty? alof) '()]
    [(cons? alof)
     (cons (cFC (first alof)) (convertFC (rest alof)))]))

; Number -> Number
; convert usd to euro
(define (cEU n)
  (* n .86))

; List-of-number -> List-of-number
; convert dollar to euro
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (cons 8 '())) (cons (cEU 8) '()))
(define (convert-euro alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon)
     (cons (cEU (first alon)) (convert-euro (rest alon)))]))

; Number -> Number
; convert usd to euro
(define (cEU* n e)
  (* n e))

; List-of-number -> List-of-number
; convert dollar to euro
(check-expect (convert-euro* '() .86) '())
(check-expect (convert-euro* (cons 8 '()) .86) (cons (cEU* 8 .86) '()))
(define (convert-euro* alon e)
  (cond
    [(empty? alon) '()]
    [(cons? alon)
     (cons (cEU* (first alon) e) (convert-euro* (rest alon) e))]))

; List-of-toys -> List-of-toys
; Replace "robot" with "r2d2"

(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))
(check-expect (subst-robot (cons "robt" '())) (cons "robt" '()))

(define (subst-robot alot)
  (cond
    [(empty? alot) '()]
    [(cons? alot)
     (cons
      (if
       (string=? (first alot) "robot")
       "r2d2"
       (first alot))
       (subst-robot (rest alot)))]))

; String String List-of-strings -> List-of-string
; Replace given string with given replacement

(check-expect (substitute "hi" "h" '()) '())
(check-expect
 (substitute
  "robot" "r2d2" (cons "robot" '())) (cons "r2d2" '()))
(check-expect
 (substitute
  "hi" "hello" (cons "robt" '())) (cons "robt" '()))


(define (substitute f r alos)
  (cond
    [(empty? alos) '()]
    [(cons? alos)
     (cons
      (if
       (string=? (first alos) f)
       r
       (first alos))
       (subst-robot (rest alos)))]))














