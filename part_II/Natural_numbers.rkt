;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Natural_numbers) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A N is one of:
; - 0
; - (add1 N)
; represents the natural numbers or the counting numbers

; COPIER
; N String -> List-of-strings
; create a list of n strings s

(check-expect (copier 2 "hello") (cons "hello" (cons "hello" '())))
(check-expect (copier 0 "hello") '())

(define (copier n str)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons str (copier (sub1 n) str))]))


(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

; (copier 0.1 "xyz")
; (copier.v2 0.1 "xyz")

; N -> Number
; computes (+ n pi) without +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n)
     (add1 (add-to-pi(sub1 n)))]))

; N N -> Number
; computes (+ n1 n2) without +
(check-within (add-wop 3 2) (+ 3 2) 0.0001)
(check-within (add-wop 3 0) (+ 3 0) 0.0001)
(check-within (add-wop 0 0) 0 0.0001)
(define (add-wop n1 n2)
  (cond
    [(zero? n1) n2]
    [(positive? n1)
     (add1 (add-wop (sub1 n1) n2))]))

; N Number -> Number
; computes (* n x) without using *
(check-expect (mult-wot 0 3) 0)
(check-expect (mult-wot 3 3) 9)
(check-expect (mult-wot 3 0) 0)
(check-expect (mult-wot 1 3) 3)

(define (mult-wot n1 x)
  (cond [(zero? n1) 0]
        [(positive? n1)
         (+ x (mult-wot (sub1 n1) x))]))
(mult-wot 3 45)

; COL
; N Image -> Image
; create a column from n copies of img

(check-expect (col 0 (empty-scene 100 100)) (empty-scene 0 0))
(check-expect (col 2 (empty-scene 100 100))
              (above (empty-scene 100 100)
                     (empty-scene 100 100)))

(define (col n img)
  (cond
    [(zero? n) (empty-scene 0 0)]
    [(positive? n)
     (above img (col (sub1 n) img))]))

; ROW
; N Image -> Image
; create a row from n copies of img

(check-expect (row 0 (empty-scene 100 100)) (empty-scene 0 0))
(check-expect (row 2 (empty-scene 100 100))
              (beside (empty-scene 100 100)
                      (empty-scene 100 100)))

(define (row n img)
  (cond
    [(zero? n) (empty-scene 0 0)]
    [(positive? n)
     (beside img (row (sub1 n) img))]))


(define SEAT
  (rectangle 10 10 "outline" "black"))

(define LHALL (overlay
               (row 8 (col 18 SEAT))
               (empty-scene 80 180)))
(define DOT
  (circle 5 "solid" "red"))

; A List-of-posns is one of:
; '()
; (cons Posn '())
; interpretation, a list of posns

; ADD-BALLOONS
; List-of-posns -> Image
; render paint on the seats
(check-expect (add-balloons '()) LHALL)
(check-expect
 (add-balloons
  (cons (make-posn 9 5) '()))
 (place-image DOT
              9 5
              LHALL))

(define (add-balloons l)
  (cond
    [(empty? l) LHALL]
    [(cons? l)
     (place-image
      DOT
      (posn-x (first l))
      (posn-y (first l))
      (add-balloons (rest l)))]))







