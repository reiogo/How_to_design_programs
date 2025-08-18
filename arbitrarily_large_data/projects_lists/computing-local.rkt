;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname computing-local) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Computing with local
(check-expect
 (inf (list 2 1 3)) 1)

(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (cond
         [(< (first l) smallest-in-rest) (first l)]
         [else smallest-in-rest]))]))

(check-expect
 (sup (list 2 1 3)) 3)

(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define largest-in-rest (sup (rest l))))
       (cond
        [(> (first l) largest-in-rest) (first l)]
        [else largest-in-rest]))]))

((local ((define (f x) (+ (* 4 (sqr x)) 3 ))) f) 1)a

;(define (f-l x) (+ (* 4 (sqr x)) 3))
;(f-l 1)