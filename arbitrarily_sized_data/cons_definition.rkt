;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cons_definition) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct pair [left right])
; A ConsPair is a structure:
; (make-pair Any Any).

; Any Any -> ConsPair
;(define (our-cons a-value a-list)
 ; (make-pair a-value a-list))

; A ConsOrEmpty is one of:
; – '()
; – (cons Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all BSL lists

; Any ConsOrEmpty -> ConsOrEmpty
(define (our-cons a-value a-list)
(cond
[(empty? a-list) (make-pair a-value a-list)]
[(pair? a-list) (make-pair a-value a-list)]
[else (error "cons: list as second argument expected")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
(if (empty? a-list)
(error 'our-first "...")
(pair-left a-list)))

; ConsOrEmpty -> ConsOrEmpty
; extracts the right part of the given pair
(define (our-rest a-list)
  (pair-right a-list))

(our-first (our-cons "a" '()))
(our-rest (our-cons "a" '()))
      
