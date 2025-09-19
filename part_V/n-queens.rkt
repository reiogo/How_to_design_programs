;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname n-queens) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; N-Queens

; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, QUEENS),
; interpretation (make-posn r c) specifies the square
; in the r-th row and c-th column

; QP QP -> Boolean
; check whether two queens threaten eachother

; data example
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2) (make-posn 1 0)  (make-posn 2 3) (make-posn 3 1)))


(define UNIT 25)
(define COLOR 'black)
(define QUEENS 8)
(define QUEEN-IMG (circle 20 "solid" "red"))

; same column
(check-expect (threatening? (make-posn 0 1) (make-posn 0 2)) #t)
; same row
(check-expect (threatening? (make-posn 1 2) (make-posn 0 2)) #t)
; left to right ascending diagonal
(check-expect (threatening? (make-posn 1 2) (make-posn 2 1)) #t)
; left to right descending diagonal
(check-expect (threatening? (make-posn 1 2) (make-posn 2 3)) #t)
; non-threatening
(check-expect (threatening? (make-posn 0 0) (make-posn 2 3)) #f)
(check-expect (threatening? (make-posn 1 2) (make-posn 3 1)) #f)

(define (threatening? q0 q1)
  (cond
    [(= (posn-x q0) (posn-x q1)) #t]
    [(= (posn-y q0) (posn-y q1)) #t]
    [(= (+ (posn-x q0) (posn-y q0))
        (+ (posn-x q1) (posn-y q1))) #t]
    [(= (- (posn-x q0) (posn-x q1))
        (- (posn-y q0) (posn-y q1))) #t]
    [else #f]))

; N [List-of QP] Image -> Image
; render i from l on an n by n board
(define (render-queens n l i)
  (cond [(empty? l) (make-board n)]
        [else
         (place-image i
                      (convert (posn-x (first l)))
                      (convert (posn-y (first l)))
                      (render-queens n (rest l) i))]))

; Number -> Image
; make chess board
(define (make-board board-length)
  (local (
          (define (make-cols n)
            (cond
              [(zero? n) (rectangle (* board-length UNIT) (* board-length UNIT) "outline" COLOR)]
              [else (add-line 
                     (make-cols (sub1 n))
                     (* n UNIT) 0 (* n UNIT) (* board-length UNIT)
                     COLOR)]))
          (define (make-rows-and-cols n)
            (cond
              [(zero? n) (make-cols board-length)]
              [else (add-line
                     (make-rows-and-cols (sub1 n) )
                     0 (* n UNIT) (* board-length UNIT) (* n UNIT)
                     COLOR)])))
    (make-rows-and-cols board-length)))

; (make-board 6)

(define (convert n)
  (+ (* n UNIT) (/ UNIT 2)))

; N -> [Maybe [List-of QP]]
; find a solution to the n queens problem

;(check-member-of
; (n-queens 4)
; (list (make-posn 0 1) (make-posn 1 3) (make-posn 2 0) (make-posn 3 2))
; (list (make-posn 0 1) (make-posn 1 3) (make-posn 3 2) (make-posn 2 0))
; (list (make-posn 0 1) (make-posn 2 0) (make-posn 1 3) (make-posn 3 2))
; (list (make-posn 0 1) (make-posn 2 0) (make-posn 3 2) (make-posn 1 3))
; (list (make-posn 0 1) (make-posn 3 2) (make-posn 1 3) (make-posn 2 0))
; 
; (list (make-posn 0 1) (make-posn 3 2) (make-posn 2 0) (make-posn 1 3))
; (list (make-posn 2 0) (make-posn 1 3) (make-posn 0 1) (make-posn 3 2)) 
; (list (make-posn 2 0) (make-posn 1 3) (make-posn 3 2) (make-posn 0 1)) 
; (list (make-posn 2 0) (make-posn 0 1) (make-posn 1 3) (make-posn 3 2)) 
; (list (make-posn 2 0) (make-posn 0 1) (make-posn 3 2) (make-posn 1 3))
; 
; (list (make-posn 2 0) (make-posn 3 2) (make-posn 1 3) (make-posn 0 1)) 
; (list (make-posn 2 0) (make-posn 3 2) (make-posn 0 1) (make-posn 1 3)))

(define (n-queens n)
  #f)

; N -> [[List-of QP] -> Boolean]
; property testing function for n-queens

(check-expect [(n-queens-solution? 4)
               (list (make-posn 2 0)
                     (make-posn 3 2)
                     (make-posn 1 3)
                     (make-posn 0 1))] #t)

(check-expect [(n-queens-solution? 4)
               (list (make-posn 0 1)
                     (make-posn 1 3)
                     (make-posn 2 0)
                     (make-posn 3 2))] #t)

(check-expect [(n-queens-solution? 4)
               (list (make-posn 2 0)
                     (make-posn 2 2)
                     (make-posn 1 3)
                     (make-posn 0 1))] #f)

(define (n-queens-solution? n)
  (lambda (solution)
    (local (; [List-of QP] -> Boolean
            (define (threat-validation loq)
              (cond
                [(empty? loq) #t]
                [else
                 (and
                  (andmap (lambda (r)
                            (not (threatening? (first loq) r)))
                          (rest loq))
                  (threat-validation (rest loq)))])))
      (and (= (length solution) n)
           (threat-validation solution)))))













