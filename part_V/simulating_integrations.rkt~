;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simulating_integrations) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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
