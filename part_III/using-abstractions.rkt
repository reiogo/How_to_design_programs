;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname using-abstractions) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Using abstractions
<<<<<<< HEAD
(build-list 3 sub1)
(filter odd? '(1 2 3 4 5 6))
(filter zero? '( 0 0 0  0 1 0 ))
(sort '( 3 2 1 4 5) >)
(sort '( 3 42 15 256 563 01 -2 ) <)
(map add1 (list 1 2 2 3 3 3))
(andmap odd? (list 1 2 3 4 5))
(andmap odd? (list 1 3 5 7))
(ormap odd? (list  2 6))
(foldr - 0 '(1 2 3 4 5 6))
(foldl - 0 '(1 2 3 4 5 6))

(argmax / '(1 2 3 4 5 6 7 8 -3))


(define-struct address [first-name last-name street])
; An Addr is a structure:
; (make-address STring String STring)
; interpretation associates a street address with a person's name

; [List-of Addr] -> [List-of String]
; extract first names from alod

(define ex0
  (list (make-address "Matthias" "Fellson" "Sunburst")
        (make-address "Robert" "Findler" "South")
        (make-address "Matthew" "Flatt" "Canyon")
        (make-address "Shriram" "Krishna" "Yellow")))

(check-expect
 (ex-ad ex0)
 '( "Matthias" "Robert" "Matthew" "Shriram"))

(define (ex-ad alod)
  (cond
    [(empty? alod) '()]
    [else
     (cons
      (address-first-name (first alod))
      (ex-ad (rest alod)))]))

; [List-of String] -> [List-of String]
; return alphabetical order

(check-expect
 (order '( "Matthias" "Robert" "Matthew" "Shriram"))
 '("Matthew" "Matthias" "Robert" "Shriram"))

(define (order alos)
  (cond
    [(empty? alos) '()]
    [else
     (insert-str
      (first alos)
      (order (rest alos)))]))

; String [List-of String] -> [List-of String]
; insert in the correct place

(check-expect
 (insert-str "hi" '("atom" "zebra"))
 '( "atom" "hi" "zebra"))

(check-expect
 (insert-str "zebra" '("atom" "hi"))
 '( "atom" "hi" "zebra"))
(check-expect
 (insert-str "atom" '("hi" "zebra"))
 '( "atom" "hi" "zebra"))

(define (insert-str s alos)
  (cond
    [(empty? alos) `( ,s)]
    [else
     (if
      (string<? s (first alos))
      (cons s alos)
      (cons (first alos)
            (insert-str s (rest alos))))]))

; [List-of String] -> String
; concats strings with space in between

(check-expect
 (cat '("atom" "hi" "zebra"))
 "atom hi zebra")

(define (cat alos)
  (cond
    [(empty? (rest alos)) (first alos)]
    [else
     (string-append
      (first alos) " "
      (cat (rest alos)))]))

; [List-of Addr] -> String
; creates a string of first names, sorted in alphabetical order,
; separated and surrounded by blank spaces

(check-expect (my-listing ex0) "Matthew Matthias Robert Shriram")

(define (my-listing l)
  (cat (order (ex-ad l))))

;; MY-BUILD-LIST ==============================================
;; N [N -> X] -> [List-of X]
;; my-build-list works just like build-list
;; recall add-at-end
;; constructs a list by applying f to 0, 1, ... (sub1 n)
;
;(check-expect
; (my-build-list 5 add1)
; (build-list 5 add1))
;
;(check-expect
; (my-build-list 5 sub1)
; (build-list 5 sub1))
;
;(define (my-build-list n f)
;  (apply-f f (reverse (rest (reverse (create-list n))))))
;
;; N -> [List-of N]
;; create a list of 0, 1, (sub1 n)
;
;(check-expect
; (create-list 2)
; '(0 1 2))
;
;(check-expect
; (create-list 3)
; '(0 1 2 3))
;
;(define (create-list n)
;    (cond
;      [(zero? n) '(0)]
;      [else
;       (insert-end n
;        (create-list (sub1 n)))]))
;
;; N [List-of N] -> [List-of N]
;; insert at the end
;
;(check-expect
; (insert-end
;  3 '( 2 3 4))
; '( 2 3 4 3))
;
;(define (insert-end n alon)
;  (cond
;    [(empty? alon) (list n)]
;    [else
;      (cons
;       (first alon)
;       (insert-end n (rest alon)))]))
;
;; [ X -> Y] [List-of X] -> [List-of Y]
;; apply f to every element of alox
;
;(define (apply-f f alox)
;  (cond
;    [(empty? alox) '()]
;    [else
;     (cons
;      (f (first alox))
;      (apply-f f (rest alox)))]))
;
;

(define (my-build-list n f)
  (cond
    [(zero? n) '()]
    [else (helper n f 0)]))

; N [N -> X] N -> [List-of X]
; i is the current number to apply f to
(define (helper n f i)
  (cond
    [(>= i n) '()]
    [else
     (cons
      (f i)
      (helper n f (add1 i)))]))

; [List-of Addr] -> String
; creates a string of first names, sorted alpha,
; separated and surrounded by blank space
(define (listing.v2 l)
  (local (; String String -> String
          ; concats two strings and prefixes with space
          (define (string-append-with-space s t)
            (string-append " " s t)))
    
    ; -- IN --
    (foldr string-append-with-space
           " "
           (sort (map address-first-name l)
                 string<?))))

; [List-of Addr] -> String
; creates a string of first names, sorted alpha,
; separated and surrounded by blank space
(define (listing.v3 l)
  (local (; String String -> String
          ; concats two strings and prefixes with space
          (define (string-append-with-space s t)
            (string-append " " s t))
          (define first-names (map address-first-name l))
          (define sorted-names (sort first-names string<?)))
    ; -- IN --
    (foldr string-append-with-space " " sorted-names)))
















