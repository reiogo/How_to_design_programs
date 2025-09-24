;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname into-acc-style) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Transforming functions into accumulator-style

; Sum ==========================================================

; [List-of Number] -> Number
; compute the sum of the numbers on alon

(check-expect (sum '(10 4 6)) 20)

(define (sum alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum (rest alon)))]))

; [List-of Number] -> Number
; compute the sum of the numbers on alon0

(check-expect (sum.v2 '(10 4 6)) 20)

(define (sum.v2 alon0)
  (local (; [List-of Number] ... -> Number
          ; compute the sum of the numbers on alon
          ; accumulator: a represents the sum of the numbers
          ; that alon lacks in comparison to alon0
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                           (+ (first alon) a))])))
    (sum/a alon0 0)))

; the accumulator is the sum of each element from the
; natural recursion.


; Factorial ==========================================================

; N -> N
; compute (* n (- n 1) (- n 2) ... 1)

(check-expect (! 3) 6)

(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

; N -> N
; compute (* n0 (- n0 1) (- n0 2) ... 1)

(check-expect (!.v2 3) 6)
(check-expect (!.v2 1) 1)

(define (!.v2 n0)
  (local (; N N -> N
          ; compute (* n (- n 1) (- n 2) ... 1)
          ; accumulator: a represents the product of
          ; multiplying the numbers from n to n0
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else
               (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))
(define (many-times f a n)
  (cond
    [(zero? n) (f a)]
    [else
     (* (f a)
        (many-times f a (sub1 n)))]))

; (time (many-times !.v2 20 1000))
; (time (many-times ! 20 1000))



; Tree ==========================================================

(define-struct node [left right])
; A Tree is one of:
; - '()
; - (make-node Tree Tree)

(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree -> N
; measure the height of abt

(check-expect (height example) 3)

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else
     (+ (max
         (height (node-left abt))
         (height (node-right abt)))
        1)]))
; Tree -> N
; measure the height of abt0

(check-expect (height.v2 example) 3)

(define (height.v2 abt0)
  (local (; Tree N -> N
          ; accumulator: a is the number of steps it takes
          ; to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
               (max (height/a (node-left abt) (add1 a))
                    (height/a (node-right abt) (add1 a)))])))
    (height/a abt0 0)))


; Tree -> N
; measure the height of abt0
(define (height.v3 abt0)
  (local (; Tree N N -> N
          ; measure the height of abt
          ; accumulator s is the number of steps
          ; it takes to reach abt from abt0
          ; accumulator m is the maximal height of
          ; the part of abt0 that is the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) (max s m)]
              [else
               (max (h/a (node-left abt) (add1 s) (add1 m))
                    (h/a (node-right abt) (add1 s) m))])))
    (h/a abt0 0 0)))


; Misc ====================================================

; [List-of Number] -> Number
; product of a list of numbers

(check-expect (product '(1 2 3)) 6)

(define (product alon0)
  (local (; [List-of Number] Number -> Number
          ; accumulator: the product of all numbers
          ; that are not in alon that are in alon0
          (define (product/a alon a)
            (cond
              [(empty? alon) a]
              [else (product/a (rest alon)
                               (* (first alon)a))])))
    (product/a alon0 1)))

; [List-of X] -> Number
; number of items on a list

(check-expect (how-many '(1 2 3)) 3)

(define (how-many alox0)
  (local (; [List-of X] Number -> Number
          ; accumulator: a is the number of
          ; elements that are not in alox but in alox0
          (define (how-many/a alox a)
            (cond
              [(empty? alox) a]
              [else
               (how-many/a (rest alox) (add1 a) )])))
    (how-many/a alox0 0)))

; N -> Number
; add n to pi without using +

(check-within (add-to-pi.v1 2) (+ 2 pi) 0.001)

(define (add-to-pi.v1 n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi.v1 (sub1 n)))]))

; N -> Number
; add n0 to pi without using +

(check-within (add-to-pi 2) (+ 2 pi) 0.001)

(define (add-to-pi n0)
  (local (; N Number -> Number
          ; accumulator: a represents the sum of 
          ; pi and the number of times 1 has been added to a.
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else
               (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n0 pi)))


; [NEList-of 1String] -> [List-of 1String]
; create a palindrome by mirror the given list around the last item

(check-expect (mirror (explode "abc")) (explode "abcba"))

(define (mirror s0)
  (local ((define abl (all-but-last s0)))
    (append
     abl
     (list (last s0))
     (reverse abl))))

; [List-of 1String] -> 1String

(check-expect (last '(1 2 3)) 3)

(define (last s0)
  (cond
    [(empty? (rest s0)) (first s0)]
    [else (last (rest s0))]))

; [List-of 1String] -> [List-of 1String]

(check-expect (all-but-last '(1 2 3)) '(1 2))

(define (all-but-last s0)
  (cond
    [(empty? (rest s0)) '()]
    [else (cons (first s0) (all-but-last (rest s0)))]))


; [NEList-of 1String] -> [List-of 1String]
; create a palindrome by mirror the given list around the last item

(check-expect (mirror.v2 (explode "abc")) (explode "abcba"))

(define (mirror.v2 s0)
  (local (; [NEList-of 1String ??? -> [List-of 1String]
          ; accumulator a represents the reversed prefix of
          ; s0 (all the letters not in s)
          (define (mirror/a s a)
            (cond
              [(empty? (rest s)) (append s0 a)]
              [else (mirror/a (rest s)
                                   (cons (first s) a))])))
    (mirror/a s0 '())))









