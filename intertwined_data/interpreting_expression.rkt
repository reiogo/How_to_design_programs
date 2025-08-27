;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting_expression) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp")) #f)))
; interpreting expressions
(define-struct add [left right])
; add is a structure:
; (make-add Number Number)
; interpretation add l and r
(define-struct mul [left right])
; mul is a structure:
; (make-mul Number Number)
; interpretation multiply l and r

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; A BSL-val is a Number

(make-add 10 -10)
(make-add (make-mul 20 3) 33)

(+ -1 2)
(+ (* -2 -3) 33)
(* (+ 1 (* 2 3)) (* 3.14 12))
(+ (* (+ 1 2) 3)
   (* (* (+ 1 1)
         2) 4))

; BSL-expr -> BSL-val
; computes an expression

(check-expect
 (eval-expression
  (make-add (make-mul 20 3) 33))
 93)
(check-expect
 (eval-expression
  (make-add (make-mul (make-add 1 2) 3)
            (make-mul (make-mul
                       (make-add 1 1)
                       2)
                      4)))
 25)
  

(define (eval-expression b-exp)
  (cond
    [(number? b-exp) b-exp]
    [(add? b-exp)
     (+
      (eval-expression (add-left b-exp))
      (eval-expression (add-right b-exp)))]
    [(mul? b-exp)
     (*
      (eval-expression (mul-left b-exp))
      (eval-expression (mul-right b-exp)))]))

(define-struct my-and [fst scnd])
; And is a structure:
; (make-and Bool-exp Bool-exp)
; interpretation (make-and x y) x and y are both true else false
(define-struct my-or [fst scnd])
; Or is a structure:
; (make-or Bool-exp Bool-exp)
; interpretation (make-or x y) x or/and y is true else false

(define-struct my-not [ele])
; Not is a structure:
; (make-not Bool-exp)
; interpretation (make-not x) x is not true else false

; A Bool-exp is one of
; - #true
; - #false
; - (make-and Bool-exp Bool-exp)
; - (make-or Bool-exp Bool-exp)
; - (make-not Bool-exp)

; A Bool-val is one of:
; - #true
; - #false

; Bool-exp -> Bool-val
; evaluate boolean expression

(check-expect
 (eval-bool-expression (make-my-and
                        (make-my-not #false)
                        (make-my-or #true #false)))
                       #true)

(define (eval-bool-expression b-exp)
  (cond
    [(my-and? b-exp)
     (and (eval-bool-expression (my-and-fst b-exp))
          (eval-bool-expression (my-and-scnd b-exp)))]
    [(my-or? b-exp)
     (or (eval-bool-expression (my-or-fst b-exp))
         (eval-bool-expression (my-or-scnd b-exp)))]
    [(my-not? b-exp)
     (not (eval-bool-expression (my-not-ele b-exp)))]
    [(boolean? b-exp) b-exp]))






















  