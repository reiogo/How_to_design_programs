;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-game) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct space-game [ufo tank])
; Space-game is a structure:
; (make-space-game Posn Number)
; interpretation (make-space-game (make-posn ux uy) tx) means that
;the UFO is currently at (ux, uy) and the tank's x-coordinate is tx.
(define g1 (make-space-game 5 5))
(space-game-ufo g1)
(space-game-tank g1)
(space-game? g1)