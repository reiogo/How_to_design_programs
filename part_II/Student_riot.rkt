;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Student_riot) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

;constraints
(define HEIGHT 180)
(define WIDTH 80)
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
               (empty-scene WIDTH HEIGHT)))
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

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of:
; - '()
; - (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n
; balloons must yet be thrown and thrown balloons landed at lob

;LIST-BALLOONS
; Number -> List-of-posns
; make a list of length n of random posns
(check-expect (list-balloons 0) '())
(check-random (list-balloons 1)
              (cons
               (make-posn
                (random WIDTH)
                (random HEIGHT)) '()))
(check-random
 (list-balloons 2)
 (cons
  (make-posn
   (random WIDTH)
   (random HEIGHT))
  (cons
   (make-posn
    (random WIDTH)
    (random HEIGHT)) '())))

(define (list-balloons n )
  (cond
    [(zero? n) '()]
    [(positive? n)
     (cons (make-posn (random WIDTH) (random HEIGHT))
           (list-balloons (sub1 n)))]))
     

; Pair -> Image
; display balloons based on pair
(define (b-draw p)
  (add-balloons (pair-lob p)))

; Pair -> Pair
; add new balloon to be displayed
(check-expect
 (b-tock
  (make-pair
   0
   '()))
 (make-pair
  0
  '()))
(check-random
 (b-tock
  (make-pair
   1
   (cons
    (make-posn 4 5)
    '())))
 (make-pair
  0
  (cons
   (make-posn (random WIDTH) (random HEIGHT))
   (cons (make-posn 4 5)
   '()))))
              
(define (b-tock p)
  (if (= (pair-balloon# p) 0)
      (make-pair 0 (pair-lob p))
  (make-pair
   (sub1 (pair-balloon# p))
   (cons (make-posn
          (random WIDTH)
          (random HEIGHT))
         (pair-lob p)))))
  

; Number -> Pair
; visualize balloon hits from a given number
(define (main n)
  (big-bang (make-pair n '())
    [on-draw b-draw]
    [on-tick b-tock 1]))
(main 5)












