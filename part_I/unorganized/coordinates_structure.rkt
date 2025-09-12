;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coordinates_structure) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Coordinate is one of:
; – a NegativeNumber
; interpretation a point on the Y axis, distance from top
(define (d1 -1))
(define (d2 -5))

; – a PositiveNumber
; interpretation a point on the X axis, distance from left
(define (d3 3))
(define (d4 5))

; – a Posn
; interpretation a point in a scene, usual interpretation
(define (d5 (make-posn 3 5)))
(define (d6 (make-posn 6 98)))