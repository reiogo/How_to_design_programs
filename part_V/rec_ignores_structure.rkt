;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rec_ignores_structure) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; recursion that ignores structure

; Four questions

; - What is a trivially solvable problem?
; -> empty list is sorted
; -> list with one element is sorted

; - How are trivial solutions solved?
; -> return the element as is, they are already solved

; - how does the algo generate new easy problems, one or many?
; -> partition the list around a pivot
; -> solve on the lt and gt lists
; -> the lt/gt are always smaller than the original list

; - Solution to smaller a solution to the original?
; - must combine lt pivot and gt

; there are log2 n instances of generate-problem.





; [List-of Number] -> [List-of Number]
; creates a list of numbers with the same number as
; alon, sorted in ascending order
; an list of one number is sorted
; termination both recursibe calls to quick-sort
; processes list that are guaranteed to miss the pivot

(check-expect (quick-sort '(11 9 2 18 12 14 4 1))
              '(1 2 4 9 11 12 14 18))
(check-expect (quick-sort '(1 1 1 12 11 ))
              '(1 1 1 11 12))

(define (quick-sort.v1 alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) (list (first alon))]
    ;[(= (length alon) 3) (reverse (sort> alon))]
    [else
     (local ((define pivot (first alon)))
       (append
        (quick-sort
         (smaller-items (rest alon) pivot))
        (list pivot)
        (quick-sort
         (larger-items (rest alon) pivot))))]))

(define (quick-sort.v2 alon)
  (cond
    [(empty? alon) '()]
    ;[(empty? (rest alon)) (list (first alon))]
    [(= (length alon) 3) (reverse (sort> alon))]
    [else
     (local ((define pivot (first alon)))
       (append
        (quick-sort
         (filter (lambda (n) (< n pivot)) (rest alon)))
        (list pivot)
        (quick-sort
         (filter (lambda (n) (>= n pivot)) (rest alon))))
       )]))

(define (quick-sort alon)
  (cond
    [(empty? alon) '()]
    ;[(empty? (rest alon)) (list (first alon))]
    [(= (length alon) 3) (reverse (sort> alon))]
    [else
     (local ((define pivot (first alon))
             ; [List-of Number]
             ;    -> [List [List-of Number] [List-of Number]]
             (define (split l)
               (cond
                 [(empty? l) (list '() '())]
                 [else
                  (local ((define cur (first l))
                          (define res (split (rest l)))
                          (define lt-list (first res))
                          (define gt-list (second res)))
                    (if (< cur pivot)
                        (list (cons cur lt-list) gt-list)
                        (list lt-list (cons cur gt-list)))
                    )]))
             (define partition (split (rest alon))))
       (append
        (quick-sort
         (first partition))
        (list pivot)
        (quick-sort
         (second partition)))
       )]))

(check-expect (quick-sort.v4 '(11 9 2 18 12 14 4 1) <)
              '(1 2 4 9 11 12 14 18))
(check-expect (quick-sort.v4 '(1 1 1 12 11 ) <)
              '(1 1 1 11 12))
(check-expect (quick-sort.v4 '(1) <)
              '(1))

(define (quick-sort.v4 alon f)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) (list (first alon))]
    [else
     (local ((define pivot (first alon))
             ; [List-of Number]
             ;    -> [List [List-of Number] [List-of Number]]
             (define (split l)
               (cond
                 [(empty? l) (list '() '())]
                 [else
                  (local ((define cur (first l))
                          (define res (split (rest l)))
                          (define lt-list (first res))
                          (define gt-list (second res)))
                    (if (f cur pivot)
                        (list (cons cur lt-list) gt-list)
                        (list lt-list (cons cur gt-list)))
                    )]))
             (define partition (split (rest alon))))
       (append
        (quick-sort
         (first partition))
        (list pivot)
        (quick-sort
         (second partition)))
       )]))

; [List-of Number] Number -> [List-of Number]
; filter for those smaller than pivot

(check-expect (smaller-items '(1 3 23) 4) '(1 3))

(define (smaller-items l pivot)
  (filter (lambda (n) (< n pivot)) l))

; [List-of Number] Number -> [List-of Number]
; filter for those larger than or equal to pivot

(check-expect (larger-items '(1 3 23) 2) '(3 23))

(define (larger-items l pivot)
  (cond
    [(empty? l) '()]
    [else
     (if
      (>= (first l) pivot)
      (cons (first l) (larger-items (rest l) pivot))
      (larger-items (rest l) pivot))]))

; 613 

; List-of-numbers -> List-of-numbers
; produces a sorted version of alon

(check-expect (sort> '(1 2 3)) '(3 2 1))
(check-expect (sort> '(-2 -2 1)) '(1 -2 -2))
(check-expect (sort> '(3 2 1)) '(3 2 1))
(check-expect (sort> '()) '())

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (cons n '())]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

; N N-> [List-of Number]
; create a list of numbers n0 long with each number less than n1

(check-expect (create-tests 0 0) '())
(check-random (create-tests 1 15) (list (random 15))) 

(define (create-tests n0 n1)
  (cond
    [(zero? n0) '()]
    [else
     (cons
      (random n1)
      (create-tests (sub1 n0) n1))]))


(define l0 (create-tests 50 15000000))

(time (sort> l0))
(time (quick-sort l0))
;(define l1 (create-tests 10 15000000))
;(time (sort> l1))
;(time (quick-sort l1))

















