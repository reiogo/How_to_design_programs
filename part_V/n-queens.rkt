;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname n-queens) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; N-Queens

; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, QUEENS),
; interpretation (make-posn r c) specifies the square
; in the r-th row and c-th column

; A Board is a one of:
; '()
; (cons QP Board)
; interpretation: a list of positions where a queen
; can still be placed

; data example
(define 4QUEEN-SOLUTION-1
  (list (make-posn 2 0)
        (make-posn 0 1)
        (make-posn 1 3)
        (make-posn 3 2)))
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2)
        (make-posn 1 0)
        (make-posn 2 3)
        (make-posn 3 1)))

(define UNIT 25)
(define COLOR 'black)
(define QUEENS 8)
(define QUEEN-IMG (circle 20 "solid" "red"))


; QP QP -> Boolean
; check whether two queens threaten eachother

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


; N -> [[List-of QP] -> Boolean]
; property testing function for n-queens
; processes [List-of QP], not Boolean.

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
      (if (false? solution)
          (error "output was false")
          (and (= (length solution) n)
               (threat-validation solution))))))

; [List-of QP] -> Boolean
; is the result equal [as a set] to either of two lists
(define (is-queens-result? x)
  (or (set=? 4QUEEN-SOLUTION-1 x) (set=? 4QUEEN-SOLUTION-2 x)))


; [List-of QP] [List-of QP] -> Boolean
; check if two lists contain the same items, regardless of order

(check-expect (set=? '(1 2) '(2 1)) #t)
(check-expect (set=? '(1 2) '(2 3)) #f)


(define (set=? l0 l1)
  (and
   (= (length l0) (length l1))
   (andmap (lambda (l0i) (member? l0i l1)) l0)))

; N -> [Maybe [List-of QP]]
; find a solution to the n queens problem

;(check-member-of
;(n-queens 4)
;(list (make-posn 0 1) (make-posn 1 3) (make-posn 2 0) (make-posn 3 2))
;(list (make-posn 0 1) (make-posn 1 3) (make-posn 3 2) (make-posn 2 0))
;(list (make-posn 0 1) (make-posn 2 0) (make-posn 1 3) (make-posn 3 2))
;(list (make-posn 0 1) (make-posn 2 0) (make-posn 3 2) (make-posn 1 3))
;(list (make-posn 0 1) (make-posn 3 2) (make-posn 1 3) (make-posn 2 0))
;(list (make-posn 0 1) (make-posn 3 2) (make-posn 2 0) (make-posn 1 3))
;
;(list (make-posn 3 2) (make-posn 2 0) (make-posn 1 3) (make-posn 0 1))
;(list (make-posn 3 2) (make-posn 2 0) (make-posn 0 1) (make-posn 1 3))
;(list (make-posn 3 2) (make-posn 0 1) (make-posn 1 3) (make-posn 2 0))
;(list (make-posn 3 2) (make-posn 0 1) (make-posn 2 0) (make-posn 1 3))
;(list (make-posn 3 2) (make-posn 1 3) (make-posn 0 1) (make-posn 2 0))
;(list (make-posn 3 2) (make-posn 1 3) (make-posn 2 0) (make-posn 0 1))
;
;(list (make-posn 0 2) (make-posn 1 0) (make-posn 2 3) (make-posn 3 1)))


(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))


(define (n-queens n)
  (place-queens (board0 n) n))

; Board N -> [Maybe [List-of QP]]
; If possible places n queens on board.
; returns a list of qp's where the queens are placed.
; Otherwise, returns #false
; generative: try combinations of qp that doesn't end in #f
; termination: If there is an answer, the first one is returned,
; if there is no answer, the possible list of qp gets exhausted
; and terminates on false

; 4 questions of generative recursion:
; 1. trivial case -> n is zero
; 2. empty list
; 3. consider all valid qp and determine whether n - 1 queens can
; be placed. Picking one possible collection of qp that
; generates a new instance of place-queens
; 4. cons the qp that resulted in a solution

(define (place-queens a-board n)
  (cond
    [(zero? n) '()]
    [else
     (local ((define candidate
               (place-queens/list
                (find-open-spots a-board) a-board n)))
       (if
        (false? candidate) #f
         candidate))]))

; [List-of QP] Board Number-> [Maybe [List-of QP]]
; finds a valid list of qp
; if there is  none, then #f
(define (place-queens/list loqp b n)
  (cond
    [(empty? loqp) #f]
    [else
     (local ((define candidate
               (place-queens
                (add-queen b (first loqp)) (sub1 n))))
       (cond
         [(false? candidate)
          (place-queens/list (rest loqp) b n)]
         [else (cons (first loqp) candidate)]))]))

; N -> Board
; creates the initial n by n board

(check-expect (board0 0) '())
(check-expect (board0 1) (list (list (make-posn 0 0))))
(check-expect (board0 2) (list (list (make-posn 0 0) (make-posn 0 1))
                               (list (make-posn 1 0) (make-posn 1 1))))
(check-expect (board0 3) (list (list (make-posn 0 0)
                                     (make-posn 0 1)
                                     (make-posn 0 2))
                               (list (make-posn 1 0)
                                     (make-posn 1 1)
                                     (make-posn 1 2))
                               (list (make-posn 2 0)
                                     (make-posn 2 1)
                                     (make-posn 2 2))))

(define (board0 n)
  (local ((define (board1 x)
            (cond
              [(zero? x) '()]
              [else
               (local ((define column (sub1 x)))
                 (cons
                  (build-list n (lambda (row) (make-posn column row)))
                  (board1 (sub1 x))))])))
    (reverse (board1 n))))

; Board QP -> Board
; places a queen at qp on a-board
; ie remove QP from Board
(check-expect (add-queen (board0 2) (make-posn 0 0))
              '())
(check-expect (add-queen (board0 3) (make-posn 0 0))
              (list (list (make-posn 1 2))
                    (list (make-posn 2 1))))
(define (add-queen a-board qp)
  (local ((define (remove b0 q0)
            (cond
              [(empty? b0) '()]
              [else
               (cons
                (filter (lambda (q1)
                          (and
                           (not (equal? q1 qp))
                           (not (threatening? q1 qp))))
                        (first b0))
                (add-queen (rest b0) q0))])))
    (filter (lambda (row) (not (empty? row))) (remove a-board qp))))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  (cond [(empty? a-board) '()]
        [else
         (append (first a-board)
                 (find-open-spots (rest a-board)))]))
















