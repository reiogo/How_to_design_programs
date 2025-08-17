;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Abstractions

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "dog" l))

; Los -> Boolean
; does l contain "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contain "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))

; Lon -> Lon
; add 1 to each number on l

(check-expect
 (add1* '( 1 2 3 4))
 '(2 3 4 5))

(check-expect
 (add1* '())
 '())
 

(define (add1* l)
  (add-to-list 1 l)
  ;  (cond
  ;    [(empty? l) '()]
  ;    [else
  ;     (cons (add1 (first l))
  ;           (add1* (rest l)))])
  )
; Lon -> Lon
; adds 5 to each number on l

(check-expect
 (plus5 '(1 2 3 4))
 '(6 7 8 9))

(check-expect
 (plus5 '())
 '())

(define (plus5 l)
  (add-to-list 5 l)
  ;  (cond
  ;    [(empty? l) '()]
  ;    [else
  ;    (cons (+ (first l) 5)
  ;          (plus5 (rest l)))])
  )

; Lon -> Lon
; adds k to every element in l

(check-expect
 (add-to-list 5 '(1 2 3))
 '(6 7 8))

(define (add-to-list k l)
  (cond
    [(empty? l) '()]
    [else
     (cons (+ k (first l))
           (add-to-list k (rest l)))]))

; Lon -> Lon
; adds -2 to every element in l

(check-expect
 (sub2 '(2 3 44))
 '(0 1 42))

(define (sub2 l)
  (add-to-list -2 l))

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (extract < l t)
  ;  (cond
  ;    [(empty? l) '()]
  ;    [else
  ;     (cond
  ;       [(< (first l) t)
  ;        (cons (first l)
  ;              (small (rest l) t))]
  ;       [else
  ;        (small (rest l) t)])])
  )

; Lon Numbers -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (extract > l t)
  ;  (cond
  ;    [(empty? l) '()]
  ;    [else
  ;     (cond
  ;       [(> (first l) t)
  ;        (cons (first l)
  ;              (large (rest l) t))]
  ;       [else
  ;        (large (rest l) t )])])
  )


(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5) (small '(3) 5))
(check-expect (extract < '(1 6 4) 5) (small '(1 6 4) 5))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x ) c))

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)

(extract squared>? '(3 4 5) 10)

; Nelon -> Number
; determines the smallest
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (max (first l) (inf (rest l)))])
  ;    (cond
  ;    [(empty? (rest l))
  ;     (first l)]
  ;    [else
  ;     (cond
  ;       [(< (first l) (inf (rest l)))
  ;        (first l)]
  ;       [else
  ;        (inf (rest l))])])
  )

; Nelon -> Number
; determines the largest
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (min (first l) (sup (rest l)))])
  ;  (cond
  ;    [(empty? (rest l))
  ;     (first l)]
  ;    [else
  ;     (cond
  ;       [(> (first l) (sup (rest l)))
  ;        (first l)]
  ;       [else
  ;        (sup (rest l))])])
  )

; Nelon R -> Number
; determines a number from a nelon depending on predicate R
(define (my-filter R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (cond
       [(R (first l) (my-filter R (rest l)))
        (first l)]
       [else
        (my-filter R (rest l))])]))

; Nelon R -> Number
; determines a number from a nelon depending on function R
; function r must return a number of the desired quality
(define (my-filter1 R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (R (first l) (my-filter1 R (rest l)))]))

(define l0 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define l1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

(define (inf-1 l)
  (my-filter < l))

(define (sup-1 l)
  (my-filter > l))

(define (inf-2 l)
  (my-filter1 min l))

(define (sup-2 l)
  (my-filter1 max l))


;(inf-1 l0)
;(inf-1 l1)
;(sup-1 l0)
;(sup-1 l1)

(inf-2 l0)
(inf-2 l1)
(sup-2 l0)
(sup-2 l1)

; A Lon is one of:
; - '()
; - (cons Number Lon)

; A Los is one of:
; - '()
; - (cons String Los)

; A [list-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])

(define-struct point [hori veri])

; A Pair-boolean-string is a structure:
; (make-point Boolean String)

; A Pair-number-image is a structure:
; (make-point Number Image)

; A [CP H V] is a structure:
; (make-point H V)

; A [List X Y] is a structure:
; (cons X (conx Y '()))

; A [List Number Number] is a structure:
; (cons Number (cons Number '()))
(define nn0 (cons 3 (cons 4 '())))

; A [List Number 1String] is a structure:
; (cons Number (cons 1String '()))
(define n1s0 (cons 3 (cons "j" '())))

; A [List String Boolean] is a structure:
; (cons String (cons Boolean '()))
(define sb0 (cons "hi" (cons #false '())))

; [List-of [CP Boolean Image]]
; [CP Number [List-of Image]]

(define-struct layer [stuff])

; A Nested-string is one of:
; - String
; - (make-layer Nested-string)
(define ns0 "hello")
(define ns1 (make-layer ns0))

; A Nested-number is one of:
; - Number
; - (make-layer Nested-number)
(define nestedn0 4)
(define nn1 (make-layer nn0))

; A [Nested ITEM] is one of:
; - ITEM
; - (make-layer [Nested ITEM])



; A NEList-of-temperatures is one of:
; - Temperature
; - (cons Temperature NEList-of-temperatures)

; A NEList-of-booleans is one of:
; - Boolean
; - (cons Boolean NEList-of-booleans)

; A [NEList-of ITEM] is one of:
; - ITEM
; - (cons ITEM [NEList-of ITEM])


(define-struct bucket [count list])
; A [Bucket ITEM] is a structure:
; (make-bucket N [List-of ITEM])
; interpretation (make-bucket n l) combines the length
; n of some list l with the list itself
; that is, (= (length l) n) is always true

(define-struct bucket-string [n los])
; A Bucket-String is a structure:
; (make-bucket-string N List-of-strings)
; interpretaion (make-bucket n l) combines the length
; n of a list of strings with the list itself
; (= (length l) n) is always true
(define bs0
  (make-bucket-string 1 '("string")))
(define bs1
  (make-bucket-string 2 '("hi" "hello")))

(define-struct bucket-ir [n losir])
; A Bucket-IR is a structure:
; (make-bucket N List-of-IR)
; a bucket with lists of IR
(define bi0
  (make-bucket-ir 1 '("ir")))
(define bi1
  (make-bucket-ir 2 '("ir0" "ir1")))

(define-struct bucket-posn [n lop])
; A Bucket-Posn is a structure:
; (make-bucket-posn N List-of-posns)
; a bucket with lists of posns
(define bp0 (make-bucket-posn
             2 '((make-posn 3 4) (make-posn 3 0))))

; [Bucket [List-of [List-of String]]]
(define blls0
  (make-bucket 2
               '(("st" "ri" "ng") ("hello"))))
(define blls1
  (make-bucket 0
               '()))
(define blls2
  (make-bucket 1
               '(("string"))))

; A [Maybe X] is one of:
; - #false
; - X

; A [Maybe String] is one of:
; - #false
; - String

; A [Maybe [List-of-String]] is one of:
; - #false
; - List-of-string

; A [List-of [Maybe String]] is one of:
; - '()
; - (cons [Maybe String] [List-of [Maybe String]])

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of the list los if it contains
; #false otherwise
(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [else
     (if
      (string=? s (first los))
      (rest los)
      (occurs s (rest los))
      )]))

(define (f x) x)

(define (eg0 x)
  (+ x 1))

(define (eg1 x)
  (if (> x 2)
      (- x 1)
      (+ x 1)))

(define (eg2 x)
  (if (> x 6)
      (- x 1)
      (+ x 1)))

; Function Function -> Boolean
; check whether two functions produce the same results
; for 1.2, 3, and 5.775

(check-expect
 (function=at-1.2-3-and--5.775? eg0 eg2) #true)

(check-expect
 (function=at-1.2-3-and--5.775? eg0 eg1) #false)

(define (function=at-1.2-3-and--5.775? f0 f1)
  (cond
    [(and
      (= (f0 1.2) (f1 1.2))
      (= (f0 3) (f1 3)) 
      (= (f0 -5.775) (f1 -5.775))
      ) #true]
    [else #false]))

(

















