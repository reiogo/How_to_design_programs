;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
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

(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons (add1 (first l))
           (add1* (rest l)))]))
; Lon -> Lon
; adds 5 to each number on l
(define (plus 5 l)
  (cond
    [(empty? l) '()]
    [else
    (cons (+ (first l) 5)
          (plus5 (rest l)))]))














