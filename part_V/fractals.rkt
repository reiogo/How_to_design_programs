;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fractals) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Fractals

(define SMALL 4) ; a size measure in terms of pixels
(define small-triangle (triangle SMALL 'outline 'red))

; Number -> Image
; generative: creates Sierpinski triangle of size side
; by generating one size side/2 and placing one copy
; above two composed copies
; terminates: the input is a positive number
; if input is smaller than SMALL, algorithm terminates,
; otherwise recursive call shrinks by half. Hence terminates
; assuming SMALL is positive

(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))

(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized
              (beside half-sized half-sized)))]))

(define TOLERANCE 0.05)

; [Number -> Number] Number Number -> Number
; determine R such that f has a root in [R, (+ R TOLERANCE)]
; assume: f is continuous
; assume: (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative: divide interval in half, the root is in
; one of the two halves, pick according to assumption
; termination: suppose the arguments for find-root describe
; an interval of size S1. S1 halves every recursive
; call, hence the distance will become less than TOLERANCE.

;(check-satisfied (find-root poly 0 3)
;                 (lambda (x) (zero? (poly x))))
;(check-satisfied (find-root poly 3 5)
;                 (lambda (x) (zero? (poly x))))
;(check-satisfied (find-root poly 0 5)
;                 (lambda (x) (zero? (poly x))))


(define (find-root f left right)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid))
             (define f@left (f left))
             (define f@right (f right)))
       (cond
         [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
          (find-root-helper f left f@left mid f@mid)]
         [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
          (find-root-helper f mid f@mid right f@right)]))]))

; [Number -> Number] Number Number Number Number -> Number
; helper function to let find-root cut recomputation of
; f outputs
(define (find-root-helper f left fleft right fright)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= fleft 0 f@mid) (<= f@mid 0 fleft))
          (find-root-helper f left fleft mid f@mid)]
         [(or (<= f@mid 0 fright) (<= fright 0 f@mid))
          (find-root-helper f mid f@mid right fright)]))]))


; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; Four fundamental questions of algorithm design:
; My attempt: 
; trivial case?
; [R, (+ R TOLERANCE)], found the case where the right
; and left is closer than a given tolerance
; or the solution to poly is ever zero
; how to solve
; just return the left coordinate
; (rounded to nearest tolerance i guess?)
; generative?
; each time the space to look for is being cut in half
; the solution to the new problem is the solution to the
; original problem

; Book:
; 1. (<= (- right left) TOLERANCE)
; 2. match for trivial case is left
; 3. expression that generates new problems for find-root.
; (local ((define mid (/ (+ left right) 2))
;    (define f@mid (f mid))) ...)
; 4. if (or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
; [left, mid]
; else if (or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
; [mid, right]
; which would be
; (local ((define mid (/ (+ left right) 2))
;    (define f@mid (f mid))) ...)
;  (cond
;    [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
;     (... (find-root f left mid) ...)]
;    [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
;     (... (find-root f mid right) ...)]))


; [Number -> Number] Number Number -> Number
; Monotonic version - assume f is monotonic

;(check-satisfied (find-root-monotonic poly 0 3)
;                 (lambda (x) (zero? (poly x))))
;(check-satisfied (find-root-monotonic poly 3 5)
;                 (lambda (x) (zero? (poly x))))
;(check-satisfied (find-root-monotonic poly 0 5)
;                 (lambda (x) (zero? (poly x))))

; halves, pick according to assumption
(define (find-root-monotonic f left right)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(<= (f left) 0 f@mid)
          (find-root f left mid)]
         [(<= f@mid 0 (f right))
          (find-root f mid right)]))]))

(define-struct table [length array])
; A Table is a structure:
; (make-table N [N -> Number])
(define table1 (make-table 3 (lambda (i) i)))

;N -> Number
(define (a2 i)
  (if (= i 0) pi
      (error "table2 is not defined for i =!= 0")))
(define table2 (make-table 1 a2))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> Number
; find the smallest index for a root of the table
; assume table is monotonically increasing

(check-expect (find-linear table1) 0)
(check-expect (find-linear table2) 0)
(check-expect (find-linear
               (make-table 3 (lambda (i) (+ i 2)))) 0)
(check-expect (find-linear
               (make-table 5 (lambda (i) (- i 3.49)))) 3)
(check-expect (find-linear
               (make-table 7 (lambda (i) (- i 3.63)))) 4)

(define (find-linear t)
  (local ((define f (table-array t))
          ; Number -> Number
          (define (func n)
            (local((define nxt (add1 n))
                   (define f@cur (f n))
                   (define last-index (sub1 (table-length t))))
              (cond
                [(= last-index n)
                 (if (< f@cur 0) n 0)]
                [(<= f@cur 0 (f nxt))
                 (if
                  (< (- (abs f@cur) (abs (f nxt))) 0)
                  n nxt)]
                [else (func nxt)]))))
    (func 0)))

; Table -> Number
; find root of a monotonically increasing function
; termination: each call cuts the range in half, terminates
; when the range is 1

(check-expect (find-binary table1) 0)
(check-expect (find-binary table2) 0)
(check-expect (find-binary
               (make-table 3 (lambda (i) (+ i 2)))) 0)
(check-expect (find-binary
               (make-table 5 (lambda (i) (- i 3.49)))) 3)
(check-expect (find-binary
               (make-table 7 (lambda (i) (- i 3.63)))) 4)

(define (find-binary t)
  (local ((define (func left right)
            (local ((define f (table-array t))
                    (define mid (floor (+ left (/ (- right left) 2))))
                    (define f@mid (f mid)))
              (cond
                [(= (- right left) 1)
                 (if (< (- (abs (f left)) (abs (f right))) 0) left right)]
                [(= f@mid 0) mid]
                [(<= (f left) 0 f@mid) (func left mid)]
                [(<= f@mid 0 (f right)) (func mid right)]
                [else (if (< (f (sub1 (table-length t))) 0) right 0)]))))
    (func 0 (sub1 (table-length t)))))
  

; trivial -> (f x) == 0, the root is found
; return the index
; generate -> check lower and upper half of lengths
; root is root





















