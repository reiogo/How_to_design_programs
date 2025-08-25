;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Match

(match (cons 's '())
  [(cons (? symbol?) tail) tail]
  [(cons head tail) head])

; A [Non-empty-list X] is one of:
; - (cons X '())
; - (cons X [Non-empty-list X])

; [Non-empty-list X] -> X
; retrieve the last item of ne-l

(check-expect (last-item '(a b c)) 'c)
(check-error (last-item '()))

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))

(define-struct layer [color doll])
; An RD is one of:
; - "wooden doll"
; - (make-layer String RD)


; RD -> Number
; how many dolls are part of anrd
(check-expect
 (depth
  (make-layer "yellow"
              (make-layer "green" "wooden doll"))) 2)
(check-expect
 (depth
  "wooden doll") 0)

(define (depth anrd)
  (match anrd
    ["wooden doll" 0]
    [(layer c inside) (add1 (depth inside))]
    ))

(define (depth-n an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd)
     (+ 1 (depth (layer-doll an-rd)))]))

; [List-of Posn] Number -> [List-of Posn]
; moves each object right by delta-x pixels

(check-expect
 (move-right (list (make-posn 1 1) (make-posn 10 14)) 3)
 (list (make-posn 4 1) (make-posn 13 14)))

(define (move-right lop delta-x)
  (map (lambda (p)
         (match p
           [(posn x y) (make-posn (+ x delta-x) y)]))
       lop))

; ShotWorld -> ShotWorld
; moves each shot up by one pixel
(define (tock w)
(cond
[(empty? w) '()]
[else (cons (sub1 (first w)) (tock (rest w)))]))

(define-struct phone [area switch four])
; A phone is a structure:
; (make-phone Number Number Number)
; interpretation, (make-phone x y z) just a phone number

; [List-of Phone] -> [List-of Phone]
; replace area code 713 with 281

(check-expect
 (replace (list (make-phone 713 333 333)))
 (list (make-phone 281 333 333)))

(check-expect
 (replace (list (make-phone 712 333 333)))
 (list (make-phone 712 333 333)))

(define (replace alop)
  (map (lambda (p)
         (match p
           [(phone x y z) (make-phone (if (= x 713) 281 x) y z)]))
       alop))


; [List-of [List-of String]] -> [List-of Number]
; determines the number of words on each line

(check-expect
 (words-on-line (list (list "atom" "abacus")(list "bat" "barn")))
 (list 2 2))
; 492

(define (words-on-line lls)
  (match lls
    [(? empty?) '()]
    [(cons fst rst) (cons (length fst) (words-on-line rst))]))
























