;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname temperatures) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

; A List-of-temperatures is one of:
; – '()
; – (cons CTemperature List-of-temperatures)
; A CTemperature is a Number greater or equal to -273.


; AVERAGE
; List-of-temperatures -> Number
; compute the average temperature
(check-expect (average (cons 1 (cons 2 ( cons 3 '())))) 2)
(define (average alot)
  (/ (sum alot)
     (how-many alot)))

; SUM
; List-of-temperatures -> Number
; adds up the temperatures on the given list
(check-expect (sum (cons 1 (cons 2 ( cons 3 '())))) 6)

(define (sum alot)
  (cond
    
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

; HOW-MANY
; List-of-temperatures -> Number
; counts the temperatures on the given list
(check-expect (how-many (cons 1 (cons 2 ( cons 3 '())))) 3)
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot)
     (+ 1 (how-many (rest alot)))]))

;CHECKED-AVERAGE
; List-of-temperatures -> Number
; Provide a check for divide by zero
(define (checked-average alot)
  (cond
    [(empty? alot) (error "cannot be an empty list")]
    [else (average alot)]))

; A NEList-of-temperatures is one of:
; - (cons CTemperature '())
; - (cons CTemperautre NEList-of-temperatures)
; interpretation non-empty lists of measured temperatures
; (cons ccc '())
; (cons -236 '())

; AVERAGE-AGAIN
; NEList-of-temperatures -> Number
; compute the average temperature
(check-expect (average-again (cons 1 (cons 2 (cons 3 '())))) 2)
(define (average-again anelot)
  (/ (sum-ne anelot)
     (how-many-ne anelot)))

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures
(check-expect (sum-ne (cons 1 (cons 2 (cons 3 '())))) 6)
(define (sum-ne anelot)
  (cond
    [(empty? (rest anelot)) (first anelot)]
    [(cons? (rest anelot))
     (+
      (first anelot ) (sum-ne (rest anelot)))]))

; SORTED>?
; NEList-of-temperatures -> Boolean
; determines whether every temperature is
; strictly more than the next number
(check-expect
 (sorted>?
  (cons 1
   (cons 2 '()))) #false)
(check-expect
 (sorted>?
  (cons 3
        (cons 2 '()))) #true)
(check-expect
 (sorted>?
  (cons 0
        (cons 3
              (cons 2 '())))) #false)
(define (sorted>? anelot)
  (cond
    [(empty? (rest anelot)) #true]
    [(cons? (rest anelot))
            (and
             (>
              (first anelot)
              (first (rest anelot)))
              (sorted>? (rest anelot)))]))

; HOW-MANY-NE
; NEList-of-temperatures -> Number
; counts the number of numbers in anelot
(check-expect
 (how-many-ne
              (cons 4 '()))
              1)
(check-expect
 (how-many-ne
  (cons 3
        (cons 2
              (cons 4 '()))))
              3)
(define (how-many-ne anelot)
  (cond
    [(empty? (rest anelot)) 1]
    [(cons? (rest anelot))
     (+ 1 (how-many-ne (rest anelot)))]))


; NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)
; interpretation, a non-empty list of booleans

; ALL-TRUE-NE
; NEList-of-booleans -> Boolean
; determine if all the booleans in anelob are true or not
(check-expect
 (all-true-ne
  (cons #true '())) #true)
(check-expect
 (all-true-ne
  (cons #false '())) #false)
(check-expect
 (all-true-ne
  (cons #true (cons #true '()))) #true)
(check-expect
 (all-true-ne
  (cons #false (cons #false '()))) #false)
(define (all-true-ne anelob)
  (cond
    [(empty? (rest anelob)) (first anelob)]
    [(cons? (rest anelob))
     (and (first anelob) (all-true-ne (rest anelob)))]))

; ONE-TRUE-NE
; NEList-of-booleans -> Boolean
; determine if any of the booleans in anelob are true or not
(check-expect
 (one-true-ne
  (cons #true '())) #true)
(check-expect
 (one-true-ne
  (cons #false '())) #false)
(check-expect
 (one-true-ne
  (cons #false (cons #true '()))) #true)
(check-expect
 (one-true-ne
  (cons #false (cons #false '()))) #false)
(define (one-true-ne anelob)
  (cond
    [(empty? (rest anelob)) (first anelob)]
    [(cons? (rest anelob))
     (or (first anelob) (one-true-ne (rest anelob)))]))












