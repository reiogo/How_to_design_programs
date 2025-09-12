;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstraction-examples) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)

; ADD-3-TO-ALL
; [List-of Posn] -> [List-of Posn]
; add 3 to the x-coord of each in lop

(check-expect
 (add-3-to-all (list (make-posn 3 5) (make-posn 5 6)))
 (list (make-posn 6 5) (make-posn 8 6)))

(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; adds 3 to the x-coord of the given Posn
          (define (add-3-to-one p)
            (make-posn (+ 3 (posn-x p)) (posn-y p))))
    (map add-3-to-one lop)))

;[X->Y] [List-of X] -> [List-of Y]

; [List-of Posn] -> [List-of Posn]
; removes posns with y-coord greater than 100

(check-expect
 (keep-good (list (make-posn 2 3) (make-posn 3 102)))
 (list (make-posn 2 3)))

(define (keep-good lop)
  (local (; Posn -> Boolean
          ; if p is less than 100
          (define (p<100? p)
          (< (posn-y p) 100)))
    (filter p<100? lop)))

; ormap
; [ X -> Boolean] [List-of X] -> Boolean

; Posn Posn Number -> Boolean
; is the distance between p and q less than d

(check-expect
 (close-to (make-posn 1 5) (make-posn 1 2) 10)
 #true)

(check-expect
 (close-to (make-posn 9 9) (make-posn 1 2) 1)
 #false)

(define (close-to p q d)
  (if
   (<= (sqrt (+ (sqr (- (posn-x p)(posn-x q)))
        (sqr (- (posn-y p)(posn-y q))))) d)
   #true
   #false))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt

(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60))
         (make-posn 50 50))
 #true)

(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))

(define CLOSENESS 5) ; in terms of pixels


























