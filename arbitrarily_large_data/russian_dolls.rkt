;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname russian_dolls) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; - String
; - (make-layer String RD)

; DEPTH
; RD -> Number
; how many dolls are part of an-rd
(check-expect
 (depth
  (make-layer "yellow"
              (make-layer "green" "red"))) 3)
(check-expect
 (depth
  "red") 1)

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd)
     (+ 1 (depth (layer-doll an-rd)))]))


; COLORS
; RD -> String
; Create a list of the colors in a RD
(check-expect
 (colors
  (make-layer "yellow"
              (make-layer "green" "red")))
 "yellow, green, red")
(check-expect
 (colors "red")
 "red")

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd)
     (string-append
      (layer-color an-rd)
      ", "
      (colors (layer-doll an-rd)))]))

; INNER
; RD -> String
; find the color of the inner most doll
(check-expect
 (inner
  (make-layer "red"
              (make-layer "green" "yellow")))
 "yellow")
(check-expect
 (inner
  "red")
 "red")
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd)
      (inner (layer-doll an-rd))]))

; (inner (make-layer "red" (make-layer "green" (make-layer "yellow" "blue"))))







