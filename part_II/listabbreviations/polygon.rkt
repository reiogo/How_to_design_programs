;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname polygon) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; POLYGON

; A Polygon is one of:
; - (list Posn Posn Posn)
; (cons Posn Polygon)

; a plain background image
(define MT (empty-scene 50 50))

;RENDER-POLY
; Polygon -> Image
; renders the given polygon p into MT

(define (render-poly p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
      (render-line
       (render-line MT (first p) (second p))
       (second p) (third p))
      (third p) (first p))]
    [else
     (render-line
      (render-poly (rest p))
      (first p) (second p))]))


; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; A NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

;CONNECT-DOTS
; NELoP -> Image
; connect the dots in p by rendering lines in MT

(check-expect
 (connect-dots
  (list (make-posn 20 0)
        (make-posn 10 10)
        (make-posn 30 10)))
 (scene+line
  (scene+line MT 20 0 10 10 "red")
  10 10 30 10 "red"))

(check-expect
 (connect-dots
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line MT 10 10 20 10 "red")
   20 10 20 20 "red")
  20 20 10 20 "red"))

(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else
     (render-line
      (connect-dots (rest p)) (first p) (second p))]))


;RENDER-POLYGON
; Polygon -> Image
; adds an image of p to MT

(check-expect
 (render-polygon
  (list (make-posn 20 0) (make-posn 10 10) ( make-posn 30 10)))
 (scene+line
  (scene+line
   (scene+line MT 20 0 10 10 "red")
   10 10 30 10 "red")
  30 10 20 0 "red"))

(check-expect
 (render-polygon
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-polygon p)
  (render-line (connect-dots p) (first p) (last p)))

; NELoP -> Posn
; extracts the last item from p

(check-expect
 (last
  (list
   (make-posn 30 30) (make-posn 4 0)))
 (make-posn 4 0))

(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else
     (last (rest p))]))

; Polygon -> Image
; adds an image of p to MT

(check-expect
 (render-poly-last
  (list (make-posn 20 0) (make-posn 10 10) ( make-posn 30 10)))
 (scene+line
  (scene+line
   (scene+line MT 20 0 10 10 "red")
   10 10 30 10 "red")
  30 10 20 0 "red"))

(check-expect
 (render-poly-last
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly-last p)
  (connect-dots (cons (last p) p)))

; Polygon -> Image
; adds an image of p to MT

(check-expect
 (render-poly-first
  (list (make-posn 20 0) (make-posn 10 10) ( make-posn 30 10)))
 (scene+line
  (scene+line
   (scene+line MT 20 0 10 10 "red")
   10 10 30 10 "red")
  30 10 20 0 "red"))

(check-expect
 (render-poly-first
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly-first p)
  (connect-dots (add-at-end (first p) p)))


; Posn NELoP -> NELoP
; Adds psn to the end of a p

(check-expect
 (add-at-end
  (make-posn 15 15)
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (list (make-posn 10 10) (make-posn 20 10)
       (make-posn 20 20) (make-posn 10 20)
       (make-posn 15 15)))

(define (add-at-end psn p)
  (cond
    [(empty? (rest p)) (list (first p) psn)]
    [else
     (cons
      (first p)
      (add-at-end psn (rest p)))]))


; CONNECT-DOTS-ACC
; Posn NELoP -> Image
; connect the dots in p by rendering lines in MT

(check-expect
 (connect-dots-acc
  (make-posn 40 10)
  (list (make-posn 20 0)
        (make-posn 10 10)
        (make-posn 30 10)))
 (scene+line
  (scene+line
   (scene+line MT 20 0 10 10 "red")
   10 10 30 10 "red")
  30 10 40 10 "red"))

(check-expect
 (connect-dots-acc
  (make-posn 50 55)
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 50 55 "red"))

(define (connect-dots-acc psn p)
  (cond
    [(empty? (rest p)) (render-line MT (last p) psn)]
    [else
     (render-line
      (connect-dots-acc psn (rest p)) (first p) (second p))]))

;RENDER-POLYGON-Acc
; Polygon -> Image
; adds an image of p to MT

(check-expect
 (render-polygon-acc
  (list (make-posn 20 0) (make-posn 10 10) ( make-posn 30 10)))
 (scene+line
  (scene+line
   (scene+line MT 20 0 10 10 "red")
   10 10 30 10 "red")
  30 10 20 0 "red"))

(check-expect
 (render-polygon-acc
  (list (make-posn 10 10) (make-posn 20 10)
        (make-posn 20 20) (make-posn 10 20)))
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-polygon-acc p)
  (connect-dots-acc (first p) p))






















