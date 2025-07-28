;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tax_land) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;Constants
(define LOW-PRICE 1000)
(define LUXURY-PRICE 10000)
; A Price falls into one of three intervals:
; —0 up to LOW-PRICE (no taxes)
; —LOW-PRICE including, and up to 10000 (5%)
; —10000 including and above. (8%)
; interpretation the price of an item

;Price -> Number
;compute the amount of tax charge for price p
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax LOW-PRICE) (* 0.05 LOW-PRICE))
(check-expect (sales-tax 1200) (* 0.05 1200))
(check-expect (sales-tax LUXURY-PRICE) (* 0.08 LUXURY-PRICE))
(check-expect (sales-tax 12017) (* 0.08 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p)(< p LOW-PRICE)) 0]
    [(and (<= LOW-PRICE p) (< p LUXURY-PRICE)) (* 0.05 p)]
    [(>= p LUXURY-PRICE) (* 0.08 p)]))
