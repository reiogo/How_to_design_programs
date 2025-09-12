;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname design-with-abstractions) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
;Designing with abstractions

;graphical constants
(define BG (empty-scene 200 200))
(define DOT (circle 5 "solid" "red"))


; [List-of Posn] -> Image
; adds the Posns on lop to the empty scene

(check-expect
 (dots (list (make-posn 12 31)))
 (place-image DOT 12 31 BG))

(check-expect
 (dots (list (make-posn 12 31) (make-posn 45 55)))
 (place-image DOT 12 31
              (place-image DOT 45 55 BG)))

(define (dots lop)
  (local (; Posn Image -> Image
          ; add one dot to scene
          (define (add-dot p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    (foldr add-dot BG lop)))

; [List-of Posn] -> Image
; adds the Posns on lop to the empty scene

(check-expect
 (dots.v2 (list (make-posn 12 31)))
 (place-image DOT 12 31 BG))

(check-expect
 (dots.v2 (list (make-posn 12 31) (make-posn 45 55)))
 (place-image DOT 12 31
              (place-image DOT 45 55 BG)))

(define (dots.v2 lop)
  (local (; Posn Image -> Image
          ; add one dot to scene
          (define (add-dot p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    (foldl add-dot BG lop)))