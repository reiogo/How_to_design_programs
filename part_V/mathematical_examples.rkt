;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mathematical_examples) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Mathematical Examples
; Newton's Method of finding roots =====================================

(define EPSILON 0.01)

; [Number -> Number] Number -> Number
; get slope of f at x

(check-expect (slope (lambda (x) x) 2) 1)
(check-expect (slope (lambda (x) 0) 2) 0)
(check-expect (slope (lambda (x) (expt x 2)) 2) 4)

(define (slope f x)
  (/ (- (f (+ x EPSILON)) (f (- x EPSILON))) (* 2 EPSILON)))


; [Number -> Number] Number -> Number
; find root of the tangent of a given function

(check-expect (root-of-tangent (lambda (x) x) 2) 0)
(check-expect (root-of-tangent (lambda (x)
                                 (+ (expt x 2) 2)) 2) 0.5)
(check-error (root-of-tangent (lambda (x)
                                (expt (- x 2) 2)) 2))

(define (root-of-tangent f x)
  (- x (/ (f x) (slope f x))))

;(slope (lambda (x) (+ (expt x 2) 2)) -2)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number] Number -> Number
; finds a number r such that (f r) is small, ie (<= (abs (f r)) EPSILON)
; generative: repeatedly generate improved guesses using f and r
; termination: When the tangent of f has a slope of zero
; newton will produce a division by zero error.
; On values where an inexact number gets calculated to infinity,
; newton will not terminate.
; On all other values, the root of the tangent gets closer
; and closer to zero, so the distance to 0 will eventually become
; smaller that epsilon


(check-within (newton poly 1) 2 EPSILON)
(check-within (newton poly 3.5) 4 EPSILON)

(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPSILON) r1]
    [else
     (newton f (root-of-tangent f r1))]))

; 4 questions of generative recursion:
; trivial -> (f r) is close enough to 0
; == (f r) is a small positibe number or a small negative number
; basically (<= (abs (f r1) EPSILON)
; solution is just r1
; generative step slightly smaller (f r); finding
; the root of the tangent of f at r1, using the root to find
; the next tangent
; The answer to the recursion is the answer to the original problem


; Number Number -> Number
; calculate how many months it takes to double
; m when the interest is n
; generative: calculate next principal and pass it recursively
; termination, as long as the rate is positive
; the principal will continue to increase, eventually
; crossing the threshold.

(check-expect
 (double-amount 1 .9) 2)
(check-expect
 (double-amount 10 .2) 4) 

(define (double-amount m n)
  (local (; Number -> Number
          (define (new-amount p)
            (+ p (* p n)))
          ; Number Number -> Number
          (define (count-months principal duration)
            (cond
              [(>= principal (* 2 m)) duration]
              [else
               (count-months (new-amount principal) (add1 duration))])))
    (count-months m 0)))

; trivial case -> the principal has doubled in amount
; return the number of months needed to reach that point
; generate the current number of months and how big the principal
; has gotten
; the final solution is the solution

; Numeric Integration =====================================
;   Kepler's Rule
;   Divide and conquer using Kepler's rule
;   Many rectangles
;   Adaptive integration based on Kepler's rule

; [Number -> Number] Number Number -> Number
; compute the area under the graph of f between a and b
; assume (< a b) holds
; generative:

(check-within (integrate-kepler
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-kepler
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
;(check-within (integrate-kepler
;              (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON) 

(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          (define (get-trapezoid l r)
            (local ((define f@l (f l))
                    (define f@r (f r))
                    (define base (- r l)))
              (+
               (* base f@r)
               (/ (* base (- f@l f@r)) 2)))))
    (+ (get-trapezoid a mid)
       (get-trapezoid mid b))))

(define INTERVAL-THRESHOLD 0.001)

; generative: split area into half until the
; threshold is crossed, then calculate with kepler
; termination, the area is halved, so the threshold
; will eventually be passed.
(check-within (integrate-dc
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-dc
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-dc
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-dc f a b)
  (local ((define mid (/ (+ a b) 2)))
    (cond
      [(<= (- b a) INTERVAL-THRESHOLD)
       (integrate-kepler f a b)]
      [else
       (+ (integrate-dc f a mid)
          (integrate-dc f mid b))])))

(define R 200)

(check-within (integrate-rectangles
               (lambda (x) 20) 12 22) 200 EPSILON)
(check-within (integrate-rectangles
               (lambda (x) (* 2 x)) 0 10) 100 EPSILON)
(check-within (integrate-rectangles
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 EPSILON)

(define (integrate-rectangles f a b)
  (local ((define (calc-rect i)
            (local (
                    (define W (/ (- b a) R))
                    (define S (/ W 2))
                    (define (rectangle i0)
                      (* W (f (+ a (* i0 W) S)))))
              (cond
                [(zero? i) (rectangle i)]
                [else
                 (+ (rectangle i) (calc-rect (sub1 i)))]))))
    (calc-rect (- R 1))))

; Gaussian Elimination =======================================

; An SOE (system of equation) is a non-empty Matrix.
; constraint: if the matrix length is n (in N), each item has length
; (+ n 1)
; interpretation: represents a system of linear equations

; An Equation is a [List-of Number].
; constraint: an Equation contains at least two numbers.
; interpretation: if (list a1 ... an b) is an Equation,
; a1, ... an are the left-hand side variable coefficients
; and b is the right-hand side

; A Solution is a [List-of Number]

; A TM (triangular matrix) is a [List-of Equation]
; such that the Equations are of decreasing length:
; n + 1, n, n-1, ... 2.
; interpretaion: represents a triangular matrix

; examples
(define M '((2 2 3 10)
            (2 5 12 31)
            (4 1 -2 1)))
(define S '(1 1 2))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; Equation Solution -> Number
; plug in sol into eq

(check-expect (plug-in (first M) S) 10)
(check-expect (plug-in (first M) '(2 2 1)) 11)

(define (plug-in eq sol)
  (local ((define (solve consts vars)
            (cond
              [(empty? consts) 0]
              [else
               (+ (* (first consts) (first vars))
                  (solve (rest consts) (rest vars)))])
            ))
    (solve (lhs eq) sol)))

; SOE Solution -> Boolean
; true if sol is the solution to asoe

(check-expect (check-solution M S) #t)
(check-expect (check-solution M '(2 2 1)) #f)
(check-expect (check-solution
               '((2 2 3 10)
                 (0 3 9 21)
                 (0 0 1 2)) S) #t)
(check-expect (check-solution
               '((2 2 3 10)
                 (0 3 9 21)
                 (0 -3 -8 -19)) S) #t)

(define (check-solution asoe sol)
  (cond
    [(empty? asoe) #t]
    [else
     (and
      (= (plug-in (first asoe) sol) (rhs (first asoe)))
      (check-solution (rest asoe) sol))]))

; Equation Equation -> Equation
; substract the scnd from fst as many times as required
; for the fst position to be 0.
; constraint: fst and scnd must be of equal length
; generative: subtract scnd from fst once and then recurse
; terminate: as long as the (first fst) is a multiple of
; (first scnd) then it will terminate

(check-expect (subtract '(2 5 12 31) '(2 2 3 10)) '(3 9 21))
(check-expect (subtract '(3 9 21) '(-3 -8 -19)) '(1 2))

(define (subtract fst scnd)
  (local (
          (define (subtract-each f s multiplier)
            (cond
              [(empty? f) '()]
              [else
               (cons
                (- (first f) (* multiplier (first s)))
                (subtract-each (rest f) (rest s) multiplier))])
            ))
    (cond
      [(zero? (first fst)) (rest fst)]
      [else
       (rest (subtract-each fst scnd (/ (first fst) (first scnd))))])))

; SOE -> TM
; triangulates the given system of equations

; 4 questions of generative recursion:
; 1. trivial case -> M is empty
; 2. solution is empty list
; 3. Divide M into top and rest. Use arithmetic on the
; rest to remove the first digit from the first column.
; Recurse on rest.
; Additionally: if the top column starts with a zero, rotate
; if leading coefficients are all 0 then signal error
; 4. cons the rows together.

(check-expect
 (triangulate M) '((2 2 3 10)
                   (3 9 21)
                   (1 2)))
(check-expect
 (triangulate
  '((2 3 3 8)
    (2 3 -2 3)
    (4 -2 2 4)))
 '((2 3 3 8)
   (-8 -4 -12)
   (-5 -5)))
(check-error
 (triangulate '((2 2 2 6)
                (2 2 4 8)
                (2 2 1 2)) "no solution"))

(define (triangulate M)
  (cond
    [(empty? M) '()]
    [else
     (local (; SOE Equation -> SOE
             (define (subtracted-matrix m0 e0)
               (cond
                 [(empty? m0) '()]
                 [else
                  (cons
                   (subtract (first m0) e0)
                   (subtracted-matrix (rest m0) e0))]))
             (define cur (first M)))
       (cond 
         [(zero? (first cur))
          (if (andmap (lambda (l) (zero? (first l))) M)
              (error "no solution")
              (triangulate (append (rest M) (list (first M)))))]
         [else
          (cons
           (first M)
           (triangulate (subtracted-matrix (rest M) (first M))))]
         ))]))

; Equation [List-of Number] -> Number
; given an equation with (+ n 1) variables and a
; list of numbers with n numbers, solve for the first variable

(check-expect (solve-for-first '(2 2 6) '(1)) 2)
(check-expect (solve-for-first '(2 2 3 10) '(1 2)) 1)

(define (solve-for-first eq alon)
  (/ (- (rhs eq) (foldr
                  (lambda (x y rst) (+ (* x y) rst))
                  0
                  (rest (lhs eq)) alon)) (first eq)))
  
; TM -> [List-of Number]
; Solves tm

(check-expect (solve (triangulate M)) '(1 1 2))

(define (solve.v1 tm)
  (cond
    [(empty? tm) '()]
    [else
     (local ((define res (solve (rest tm))))
       (cons
        (solve-for-first (first tm) res)
        res))]))

(define (solve tm)
  (foldr (lambda (r rst) (cons (solve-for-first r rst) rst)) '() tm))

; SOE -> [List-of Number]
; solves a matrix

(check-expect (gauss M) '(1 1 2))

(define (gauss m)
  (solve (triangulate m)))


























