;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mathematical_examples) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Mathematical Examples
; Newton's Method of finding roots =====================================

(define EPSILON 0.01)

; [Number -> Number] Number -> Number
; get slope of f at x

(check-expect (slope (lambda (x) x) 2) 1)
(check-expect (slope (lambda (x) 0) 2) 0)
(check-expect (slope (lambda (x) (expt x 2)) 2) 4)

(define (slope f x)
  (/ (- (f (+ x EPSILON)) (f (- x EPSILON))) (* 2 EPSILON)))


; [Number -> Number] Number -> Number
; find root of the tangent of a given function

(check-expect (root-of-tangent (lambda (x) x) 2) 0)
(check-expect (root-of-tangent (lambda (x)
                                 (+ (expt x 2) 2)) 2) 0.5)
(check-error (root-of-tangent (lambda (x)
                                (expt (- x 2) 2)) 2))

(define (root-of-tangent f x)
  (- x (/ (f x) (slope f x))))

;(slope (lambda (x) (+ (expt x 2) 2)) -2)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number] Number -> Number
; finds a number r such that (f r) is small, ie (<= (abs (f r)) EPSILON)
; generative: repeatedly generate improved guesses using f and r
; termination: When the tangent of f has a slope of zero
; newton will produce a division by zero error.
; On values where an inexact number gets calculated to infinity,
; newton will not terminate.
; On all other values, the root of the tangent gets closer
; and closer to zero, so the distance to 0 will eventually become
; smaller that epsilon


(check-within (newton poly 1) 2 EPSILON)
(check-within (newton poly 3.5) 4 EPSILON)

(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPSILON) r1]
    [else
     (newton f (root-of-tangent f r1))]))

; 4 questions of generative recursion:
; trivial -> (f r) is close enough to 0
; == (f r) is a small positibe number or a small negative number
; basically (<= (abs (f r1) EPSILON)
; solution is just r1
; generative step slightly smaller (f r); finding
; the root of the tangent of f at r1, using the root to find
; the next tangent
; The answer to the recursion is the answer to the original problem


; Number Number -> Number
; calculate how many months it takes to double
; m when the interest is n
; generative: calculate next principal and pass it recursively
; termination, as long as the rate is positive
; the principal will continue to increase, eventually
; crossing the threshold.

(check-expect
 (double-amount 1 .9) 2)
(check-expect
 (double-amount 10 .2) 4) 

(define (double-amount m n)
  (local (; Number -> Number
          (define (new-amount p)
            (+ p (* p n)))
          ; Number Number -> Number
          (define (count-months principal duration)
            (cond
              [(>= principal (* 2 m)) duration]
              [else
               (count-months (new-amount principal) (add1 duration))])))
    (count-months m 0)))

; trivial case -> the principal has doubled in amount
; return the number of months needed to reach that point
; generate the current number of months and how big the principal
; has gotten
; the final solution is the solution

; Numeric Integration =====================================
;   Kepler's Rule
;   Divide and conquer using Kepler's rule
;   Many rectangles
;   Adaptive integration based on Kepler's rule

; [Number -> Number] Number Number -> Number
; compute the area under the graph of f between a and b
; assume (< a b) holds
; generative:

(check-within (integrate-kepler
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-kepler
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-kepler
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON) 

(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (get-trapezoid l r)
            (local ((define f@l (f l))
                    (define f@r (f r))
                    (define base (- r l)))
              (+
               (* base f@r)
               (/ (* base (- f@l f@r)) 2)))))
    (+ (get-trapezoid a mid)
       (get-trapezoid mid b))))

(define INTERVAL-THRESHOLD 0.001)

; generative: split area into half until the
; threshold is crossed, then calculate with kepler
; termination, the area is halved, so the threshold
; will eventually be passed.
(check-within (integrate-dc
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-dc
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-dc
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-dc f a b)
  (local ((define mid (/ (+ a b) 2)))
    (cond
    [(<= (- b a) INTERVAL-THRESHOLD)
     (integrate-kepler f a b)]
    [else
     (+ (integrate-dc f a mid)
        (integrate-dc f mid b))])))

(define R 200)

(check-within (integrate-rectangles
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-rectangles
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-rectangles
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-rectangles f a b)
  (local ((define (calc-rect i)
            (local (
                    (define W (/ (- b a) R))
                    (define S (/ W 2))
                    (define (rectangle i0)
                      (* W (f (+ a (* i0 W) S)))))
              (cond
                [(zero? i) (rectangle i)]
                [else
                 (+ (rectangle i) (calc-rect (sub1 i)))]))))
    (calc-rect (- R 1))))















