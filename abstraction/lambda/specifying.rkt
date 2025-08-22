;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname specifying) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Specifying with lambda



; [X X -> Boolean] -> [[List-of X] -> Boolean]
; produces a function that determines whether
; some list is sorted according to cmp

(check-expect
 [(sorted string<?) '()] #true)

(check-expect
 [(sorted string<?) '("a" "b" "c")] #true)

(check-expect
 [(sorted <) '(1 2 3 4 5 6 7 8)] #true)

(check-expect
 [(sorted <) '(1 2 3 4 5 6 8 7)] #false)

(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is the given list l0 sorted according to cmp
            (define (sorted/l l)
              (cond [(empty? (rest l)) #true]
                    [else
                     (and
                      (cmp (first l) (second l))
                      (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

; [X X -> Boolean] [NEList-of X] -> Boolean
; determine whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (sorted? cmp l)
  (cond [(empty? (rest l)) #true]
        [else
         (and
          (cmp (first l) (second l))
          (sorted? cmp (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort-cmp/bad l)
  '(9 8 7 6 5 4 3 2 1 0))

;(check-expect (sort-cmp/worse '(1 2 3)) '(1 2 3))
(check-satisfied (sort-cmp/worse '(1 2 3))
                 (sorted-variant-of '(1 2 3) <))

(define (sort-cmp/worse l)
  (local ((define sorted-version (sort l <)))
    (cons (- (first sorted-version) 1) sorted-version)))

; [List-of X] [X X -> Boolean] -> [ [List-of X ] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
; (define a-list (generate-a-list-of-random-numbers 500))
;(check-satisfied (sort-cmp a-list <) (sorted-variant-of.v2 a-list <))

(check-expect
 [(sorted-variant-of '(1 3 2) <) '(1 2 3)] #t)

(check-expect
 [(sorted-variant-of '(1 3 2) <) '(1 3)] #f)

(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         )))

(define (sorted-variant-of.v2 k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? k l0)
         )))
; [List-of X] [List-of X] -> Boolean
; are all items in list k members of list l

(check-expect (contains? '(1 2 3) '(2 1 4 3)) #f)
(check-expect (contains? '(1 2 3 4) '(2 1 3)) #t)

(define (contains? l k)
  (andmap (lambda (item-in-k) (member? item-in-k l)) k))

; ===============================================================
; FIND FOUND?

; X [List-of X] -> [Maybe [List-of X]]
; produces the first sublist of l that
; starts with x, #false otherwise

(check-expect
 (find 4 '(1 2 3 4 5))
 '(4 5))

(check-expect
 (find 4 '(1 2 3 5))
 #f)

(check-satisfied
 (find 4 '(1 2 3 4 5))
 (found? 4 '(1 2 3 4 5) =))


(define (find x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l ) x) l (find x (rest l)))]))

; X [List-of X]  -> Boolean
; given a list of elements and evaluate if it is the found
; of the given string.
; - Is the search element in the list,
; - Is the returned list a subset of the list
; - Does the result start with x
; - Is it the first occurence of x


(check-expect
 [(found? 4 '(1 2 3 4 5) =) '(4 5)] #t)

(check-expect
 [(found? 4 '(1 2 3 5) =) #false] #t)

(check-expect
 [(found? 4 '(1 2 3 4 5) =) '(5)] #f)

(check-expect
 [(found? 4 '(1 2 3 4 5) =) #false] #f)

(define (found? x l0 eq)
  (lambda (l1)
    (local (; X [List-of X] -> Boolean
            ; check if the list contains x
            (define (contains? x0 alox)
              (ormap (lambda (m) (eq x0 m)) alox))
            ; [List-of X] X -> [List-of X]
            ; get rest of list from given x1
            (define (cut-til alox x0)
              (cond
                [(empty? alox) '()]
                [else
                 (if
                  (not (eq (first alox) x0))
                  (cons (first alox) (cut-til (rest alox) x0))
                  '())]))
            ; X [List-of X] -> Boolean
            ; Starts with x
            (define (starts-with? x0 lox0)
              (eq (first lox0) x0))
            ; [List-of X] -> Boolean
            ; is l1 a contiguous subset of alox
            (define (contiguous-subset? alox)
              (cond
                [(empty? alox) #f]
                [else
                 (if
                  (eq x (first alox))
                  (equal? l1 alox)
                  (contiguous-subset? (rest alox)))]))
            ;First-sublist
            ; [List-of X] [List-of X] -> Boolean
            (define (first-sublist? lox0 lox1)
              (not (member? x (cut-til lox1 x))))
            )
      (cond
        [(false? l1) (not (contains? x l0))]
        [else
         (and
          (starts-with? x l1)
          (contiguous-subset? l0)
          (first-sublist? l1 l0))]))))

; ===============================================================
; IS-INDEX? 

; X [List-of X] -> [Maybe N]
; determing the 0-based index of the first occurence of x in l,
; #false otherwise

(check-expect
 (index 3 '(1 2 5 6 4 3)) 5)
(check-satisfied
 (index 3 '(1 2 5 6 4 3)) (is-index? 3 '(1 2 5 6 4 3)))

(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> [Number -> Boolean]
; produces a function that determines if n0
; is the index of x in l0
;index properties are
; - x is in l0
; - the place of the index is correct
; - it is the first occurence of x in l0


(check-expect
 [(is-index? 3 '(1 2 5 6 4 3)) 5] #t)
(check-expect
 [(is-index? 3 '(1 2 5 6 4 3)) 4] #f)
(check-expect
 [(is-index? 3 '(1 2 5 6 4)) #t] #f)
(check-expect
 [(is-index? 3 '(1 2 5 6 4)) 5] #f)
(check-expect
 [(is-index? 3 '(1 2 5 6 4)) #f] #t)

(define (is-index? x l0)
  (lambda (n0)
    (local (;[List-of X] Number -> Boolean
            ; whether index n1 of alox is x0
            (define (ith-index-is alox x0)
              (cond [(empty? alox) #f]
                    [else
                     (or
                      (and (equal? x (first alox)) (= x0 0))
                      (ith-index-is (rest alox) (sub1 x0)))]))
            ; [List-of X] Number -> Boolean
            ; Whether x0 shows up before the given index
            (define (not-occur-before alox x0)
              (cond [(empty? alox) #f]
                    [(<= x0 0) #t]
                    [else
                     (if
                      (and (equal? x (first alox)) (> x0 0))
                      #f
                      (not-occur-before (rest alox) (sub1 x0)))]))
            )
      (cond
        [(false? n0)
         (if (member? x l0)
             #f #t)]
        [else
         (and
          (member? x l0)
          (ith-index-is l0 n0)
          (not-occur-before l0 n0)
          #t
          )]))))


; ===============================================================
; N-INSIDE-PLAYGROUND?

; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)

; N -> [List-of Posn]
; generate n random Posns in a WIDTH by HEIGHT rectangele
(check-satisfied (random-posns 3) (n-inside-playground? 3))

(define (random-posns n)
  (build-list n (lambda (i) (make-posn (random WIDTH) (random HEIGHT)))))


; Number -> [[List-of Posn] -> Boolean]
; returns a predicate that determines
; lop has the correct number of posns and are all within bounds.

(check-expect
 [(n-inside-playground? 3) (list
                            (make-posn 3 5)
                            (make-posn 299 299)
                            (make-posn 3 5))] #true)

(check-expect
 [(n-inside-playground? 3) (list
                            (make-posn 3 5)
                            (make-posn 299 299))] #f)

(check-expect
 [(n-inside-playground? 3) (list
                            (make-posn 3 5)
                            (make-posn 301 299)
                            (make-posn 59 49))] #f)



(define (n-inside-playground? k)
  (lambda (l0) (local (; [List-of Posn] -> Boolean
                      ; Check that all posns are within bounds
                      (define (within-playground alop)
                        (andmap
                         (lambda (p)
                           (and
                            (<= 0 (posn-x p) WIDTH)
                            (<= 0 (posn-y p) HEIGHT)
                            )) alop))
                      ; Number [List-of Posn] -> Boolean
                      ; There are k posns
                      (define (k-posns n alop)
                        (= k (foldr (lambda (p num) (add1 num)) 0 alop))))
                (and
                 (within-playground l0)
                 (k-posns k l0)))))

; Number -> [List-of Posn]
; generate n random Posns in a WIDTH by HEIGHT rectangle
; that is incorrect but should pass n-inside-playground?
(check-satisfied
 (random-posns/bad 1) (n-inside-playground? 1))
(define (random-posns/bad n)
  (list
   (make-posn 3 4)))









