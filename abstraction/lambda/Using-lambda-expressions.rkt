;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Using-lambda-expressions) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
;; Lambda
;((lambda (x y) (x y y)) + 4)
;((lambda (x) x) 10)
;((lambda (x y) x) 39 9)
;
;((lambda (x y)
;   (+ x (* x y)))
; 1 2)
;
;((lambda (x y)
;   (+ x
;      (local ((define z (* y y)))
;        (+ (* 3 z)
;           (/ 1 x)))))
; 1 2)
;
;((lambda (x y)
;   (+ x
;      ((lambda (z)
;         (+ (* 3 z)
;            (/ 1 z)))
;       (* y y))))
; 1 2)
; 
;((lambda (x) (> x 10)) 11)
;
;((lambda (x y) (number->string (* x y))) 3 4)
;
;
(define-struct ir [ name price])
;((lambda (x y) (> (ir-price x) (ir-price
;                                y)))
; (make-ir 3) (make-ir 2))
;
;
;((lambda (n) (modulo n 2)) 4)
;
;(define DOT (circle 3 "solid" "red"))
;
;((lambda (p im) (place-image DOT (posn-x p) (posn-y p) im))
; (make-posn 3 4) (empty-scene 100 100))

;(define (f-plain x)
;  (* 10 x))
;(define f-lambda
;  (lambda (x)
;    (* 10 x)))
;
;; Number -> Boolean
;(define (compare x)
;  (= (f-plain x) (f-lambda x)))
;
;(compare (random 100000))
;(f-plain (f-plain 42))
;
;
;
;((lambda (x)
;    (* 10 x)) ((lambda (x)
;    (* 10 x)) 42))

;((lambda (x) (* 10 x))2)
;((lambda (name rst) (string-append name ", " rst)) "Robert" "etc.")
(define threshold 12)
;((lambda (ir) (<= (ir-price ir) threshold)) (make-ir 10))

;(map (lambda (x) (* 10 x))
 ;    '(1 2 3))

;(foldl (lambda (name rst) (string-append name ", " rst)) "etc."
 ;      '("Matthew" "Robby"))
;(filter (lambda (ir) (<= (ir-price ir) threshold))
;        (list (make-ir "bear" 10) (make-ir "dol" 33)))

;((lambda (x) (x x)) (lambda (x) (x x)))






