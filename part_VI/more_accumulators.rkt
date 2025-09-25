;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname more_accumulators) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; More Uses of Accumulation

; A Lam is one of:
; - a Symbol
; - (list 'l (list Symbol) Lam)
; - (list Lam Lam)

(define ex1 '(l (x) x))
(define ex2 '(l (x) y))
(define ex3 '(l (y) (l (x) y)))
(define ex4 '((l (x) (x x)) (l (x) (x x))))
(define ex5 '(((l (y) (l (x) y)) (l (z) z)) (l (w) w)))

; X -> Boolean
; is it a Lam?
(define (is-lam? x)
  (or
   (is-var? x)
   (is-l? x)
   (is-app? x)))

; X -> Boolean
; is it a variable?
(define (is-var? x)
  (symbol? x))

; X -> Boolean
; is it l?
(define (is-l? x)
  (and
   (and (list? x) (= (length x) 3))
   (symbol=? (first x) 'l)
   (and (list? (second x)) (andmap (lambda (s) (symbol? s)) (second x)))
   (is-lam? (third x))))

; X -> Boolean
; is it l?
(define (is-app? x)
  (and
   (and (list? x) (= (length x) 2))
   (is-lam? (first x))
   (is-lam? (second x))))

; [X -> Y] -> [List-of Symbol]
; extract params from a lambda expression
(define (l-para x)
  (second x))

; [X -> Y] -> [List Lam Lam]
; extract body from a lambda expression
(define (l-body x)
  (third x))

; [X -> Y] -> Lam
; extract function from an application
(define (app-fun x)
  (second x))

; [X -> Y] -> Lam
; extract argument from an application
(define (app-arg x)
  (first x))


; [X -> Y] -> [List-of Symbol]
; list of all symbols used as lambda parameters in a lambda term
; duplicate symbols remain
(define (declareds x)
  ...)




















