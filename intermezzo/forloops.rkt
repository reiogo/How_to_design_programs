;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname forloops) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; For loops
(for/list ((i 10))
  i)
(build-list 10 (lambda (i) i))

(for/list((i 2) (j '(a b)))
  (list i j))

(local ((define i-s (build-list 2 (lambda (i) i)))
        (define j-s '(a b)))
  (map list i-s j-s))

; [List-of X] -> [List of [List N X]]
; create a list with the indices

(check-expect
 (enumerate '( "a" "b" "c"))
 '( (1 "a") (2 "b") (3 "c")))

(check-expect
 (enumerate '())
 '())

(define (enumerate-a alox)
  (local ((define i-s (build-list (length alox) (lambda (i) i))))
    (map list i-s alox)))

(define (enumerate alox)
  (for/list((item alox) (ith (length alox)))
    (list (+ ith 1) item)))

(for*/list ((i 2) (j '(a b)))
  (list i j))
(for/list ((i 2))
  (for/list ((j '(a b)))
    (list i j)))

; [List-of X] [List-of X] -> [List-of [List X X]]
; list of pairs from l1 and l2



(define (cross-a l1 l2)
  (foldr
   (lambda (a b) (append a b)) '()
   (map (lambda (x) (map (lambda (a) (list a x)) l1)) l2 )))

(check-satisfied
 (cross-f '(a b c) '(1 2)) (lambda (c) (= (length c) 6)))

(define (cross-f l1 l2)
  (for*/list ([item1 l1][item2 l2])
    (list item1 item2)))

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else (for*/list ([item w]
                      [arrangement-without-item
                       (arrangements (remove item w))])
            (cons item arrangement-without-item))]))

; test:
; [List-of X] -> Boolean
(define (all-words-from-rat? w)
  (and (member? (explode "rat") w)
       (member? (explode "art") w)
       (member? (explode "tar") w)))

(check-satisfied (arrangements '("r" "a" "t")) all-words-from-rat?)

(define width 2)
(for/list ([width 3][height width])
  (list width height))

(for*/list ([width 3][height width])
  (list width height))

(for/and ((i 10)) (> (- 10 i) 0))
(for/and ((i 10)) (if (>= i 0) i #false))
; X [X->[Maybe X]] -> [Maybe X]
; goes from 0 to n-1 and checks if a predicate applies to all
; else #f

(check-expect
 (and-map 10 (lambda (i) (> (- 10 i) 0)))
 (for/and ((i 10)) (> (- 10 i) 0)))

(check-expect
 (and-map 10 (lambda (i) (if (>= i 0) i #f)))
 (for/and ((i 10)) (if (>= i 0) i #f)))

(define (and-map i func)
  (foldl
   (lambda (a b) (if (false? b) #f (func a)))
   #t
   (build-list i (lambda (n) n))))
 
(for/or ((i 10)) (if (= (- 9 i) 0) i #f))
(for/or ((i 10)) (if (<= i 0) i #f))

; X [X->[Maybe X] -> [Maybe X]
; returns the first valid value if all invalid then false

(check-expect
 (or-map 10 (lambda (i) (if (= (- 9 i) 0) i #f)))
 (for/or ((i 10)) (if (= (- 9 i) 0) i #f)))

(check-expect
 (or-map 10 (lambda (i) (if (<= i 0) i #f)))
 (for/or ((i 10)) (if (<= i 0) i #f)))

(define (or-map i func)
  (foldr
   (lambda (a b) (if (false? (func a)) b (func a)))
   #t
   (build-list i (lambda (n) n))))

(for/sum ((c "abc")) (string->int c))

; X [X->Y] -> Y
; sum all elements from 0 to n-1 or all lists

(check-expect
 (my-for-sum 10 (lambda (i) i))
 (for/sum ((c 10)) c))

(define (my-for-sum i func)
  (foldr (lambda (x y) (+ (func x) y)) 0
         (build-list i (lambda (i) i))))

(for/product ((c "abc")) (+ (string->int c) 1))

; X [X -> Y] -> Y
; multiply all elements in a given string

(check-expect
 (my-for-product-string "abc" (lambda (i) (+ (string->int i) 1)))
 (for/product ((c "abc")) (+ (string->int c) 1)))

(define (my-for-product-string i func)
  (foldr (lambda (x y) (* (func x) y)) 1
         (explode i)))

(define a (string->int "a"))
(for/string ((j 10)) (int->string (+ a j)))

; X [X -> Y] -> String
; makes a list from a given value and function

(check-expect
 (my-for-string 10 (lambda (i) (int->string (+ a i))))
 (for/string ((j 10)) (int->string (+ a j))))

(define (my-for-string i func)
  (implode
   (foldr
    (lambda (m a) (cons (func m) a))
    '()
    (build-list i (lambda (i) i)))))

(define (enumerate.v2 l)
  (for/list ((item l) (ith (in-naturals 1)))
    (list ith item)))

; N -> Number
; add the even numbers between 0 and n (exclusive)
(check-expect (sum-evens 2) 0)
(check-expect (sum-evens 4) 2)
(define (sum-evens n)
  (for/sum ([i (in-range 0 n 2)]) i))

; [List-of Number] -> [List-of Number]
(define RATE 1.08)
; convert euro to dollar

(check-expect (convert-euro '(10 12))
              (list (* RATE 10) (* RATE 12)))

(define (convert-euro n)
  (for/list ([i n]) (* RATE i)))


; N -> [List-of N]
; create list from 0 to n-1

(check-expect
 (create-0-to-sub1-n 10)
 '(0 1 2 3 4 5 6 7 8 9 ))

(define (create-0-to-sub1-n n)
  (for/list ([i (in-range 0 n 1)]) i))


; N -> [List-of N]
; create list from 1 to n

(check-expect
 (create-0-n 3)
 '(1 2 3))

(define (create-0-n n)
  (for/list ([i (in-range 1 (+ n 1) 1)]) i))

; N -> [List-of N]
; create list of 1, 1/10, 1/100 ... up to n long

(check-expect
 (div10-list 3)
 (list 1 1/10 1/100))

(define (div10-list n)
  (for/list ([i (in-range 0 n 1)]) (/ 1 (expt 10 i))))

; N -> [List-of N]
; list of even numbers first n

(check-expect
 (even-list 3)
 '(0 2 4 ))

(check-expect
 (even-list 4 )
 '(0 2 4 6))


(define (even-list n)
  (for/list ([i (in-range 0 (* 2 n) 2)]) i))

; N -> [List-of [List-of N]];
; create an identity matrix of size n

(check-expect
 (iden-matrix 1)
 (list (list 1)))

(check-expect
 (iden-matrix 2)
 (list (list 1 0) (list 0 1)))

(check-expect
 (iden-matrix 3)
 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(check-expect
 (iden-matrix 4)
 (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

(define (iden-matrix n)
  (for/list ([i (in-range 0 n 1)])
    (for/list ((m (in-range 0 n 1))) (if (= m i) 1 0))
    )
  )

(define (tabulate-a f n)
  (build-list n f))

(check-expect
 (tabulate-f add1 5)
 (tabulate-a add1 5))

(define (tabulate-f f n)
  (for/list ([i (in-range 0 n 1)]) (f i)))

; String String -> Boolean
; check if s1 extends s0

(check-expect
 (string-extends? "hi" "hii") #t)

(check-expect
 (string-extends? "hi" "hei") #f)


(define (string-extends? s0 s1)
  (and
   (<= (string-length s0) (string-length s1))
   (for/and ([i s0][j s1]) (string=? i j))))



; String [List-of String] -> String
; retrieve first name that is equal to or an extension of s0 on l0

(check-expect (find-name "hi" '( "hi" "hello"))
              "hi")

(check-expect (find-name "hi" '( "hii" "hello"))
              "hii")

(check-expect (find-name "hi" '( "eii" "hello"))
              #f)


(define (find-name s0 l0)
  (for/or ([i l0]) (if (string-extends? s0 i) i #f)))


; Number [List-of String] -> Boolean
; no name exceeds a w

(check-expect
 (notexceed 2  '( "hii" "hello")) #f)

(check-expect
 (notexceed 5  '( "hii" "hello")) #t)

(define (notexceed w los)
  (for/and ([i los]) (<= (string-length i) w)))

























