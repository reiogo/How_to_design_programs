;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname algorithm_design) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Algorithm Design
; 1. the trivial case is the new random is not the same as
; the original posn
; 2. return the new random
; 3. try again
; 4. the solution is the same as one of the new solutions

(define MAX 2)
; Posn -> Posn
; return a new posn that is not the same as before
; idea keep making a random posn until it doesn't overlap
; with the one from before
; termination: If there is at least one other possible
; posn then, the function will probabilistically terminate
; (MAX * MAX -1) / (MAX * MAX) probability that it will
; not be the same as the original, allowing the function to terminate

; termination: If the random generated values create
; a posn that overlaps with the original then it will not
; terminate (food-create (make-posn 1 1) and the random
; posn is (make-posn 1 1)

(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (local ((define (food-check-create p candidate)
            (if (equal? p candidate)
                (food-create p)
                candidate)))
    (food-check-create p (make-posn (random MAX) (random MAX)))))

; Posn Posn -> Posn
; generative recursion
; If the posns are the same try again
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

(check-expect (special '(1 2 4 "a" "b"))
              '(5 (-1 -2 -4 "A" "B")))

(define (general P)
  (cond
    [(
      ;trivial?
      empty?
      P) (solve P)]
    [else
     (combine-solutions
      P
      (general (rest
                ;generate
                P)))]))

(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
      P
      (special (rest P)))]))

(define (solve i)
  '(0 ()))

(define (combine-solutions i0 i1)
  (cond
    [(string? (first i0))
     (list
      (add1 (first i1))
      (cons (string-upcase (first i0)) (second i1)))
     ]
    [(number? (first i0))
     (list
      (add1 (first i1))
      (cons (- 0 (first i0)) (second i1)))]))


; N[>= 1] N[>=1] -> N
; finds the greatest common divisor of n and m

(check-expect (structural-gcd 6 25) 1)
(check-expect (structural-gcd 18 24) 6)

(define (structural-gcd n m)
  (local (
          ; N[>=1] -> [List-of N]
          ; find all the divisors of x
          (define (cd-list x)
            (local (
                    ; N[>=1] N[>=1] -> [List-of N]
                    (define (cd-list-internal y n)
                      (cond
                        [(= n y) (list y)]
                        [else
                         (if
                          (= 0 (modulo y n))
                          (cons n (cd-list-internal y (add1 n)))
                          (cd-list-internal y (add1 n)))
                          ])))
              (cd-list-internal x 1)))
            (define cd-listm (cd-list m))
            (define cd-listn (cd-list n)))
          (first
           (sort
            (filter
             (lambda (cd) (member? cd cd-listm))
             cd-listn)
            >))))

(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

(define (gcd-structural small large)
  (largest-common (divisors small small) (divisors small large)))

; N[>= 1] N[>= 1] -> [List-of N]
; computes the list of divisors of l smaller or equal to k

(define (divisors l k)
  (filter (lambda (n) (zero? (modulo k n))) (build-list l add1)))

;[List-of N] [List-of N] -> N
; finds the largest number common to both k and l
(check-expect (largest-common '(1 2 19) '(19 18 21)) 19)
(define (largest-common k l)
  (first (sort (filter (lambda (n) (member? n l)) k) >)))

(define (gcd-structural.v1 n m)
  (local (; N -> N
          ; determines the greatest divisor of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (=
                    (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

;(time (gcd-structural 101135853 45014640))


(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)

(define (gcd-generative n m)
  (local (; N[>=1] N[>=1] -> N
          ;generative recursion
          ; (gcd large small) == (gcd small (remainder large small))
          (define (clever-gcd large small)
            (cond
              [(= small 0) large]
              [else (clever-gcd small (remainder large small))])))
    (clever-gcd (max m n) (min m n))))

;(time (gcd-structural 101135853 45014640))
;(time (gcd-generative 101135853 45014640))















  