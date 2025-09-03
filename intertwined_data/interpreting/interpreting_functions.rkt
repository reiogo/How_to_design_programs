;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting_functions) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp")) #f)))
; interpreting functions

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

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

(define-struct func [name expr])
; A func is a structure:
; (make-func string BSL-func-expr)
; interpretation (make-func n e)
; n is the name of expression e

; A BSL-func-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-func-expr BSL-func-expr)
; - (make-mul BSL-func-expr BSL-func-expr)
; - (make-func name BSL-func-expr)

(make-func 'k (make-add 1 1))
(make-mul 5 (make-func 'k (make-add 1 1)))
(make-mul (make-func 'i 5) (make-func 'k (make-add 1 1)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; take an expression and replace all x's with v's

(check-expect
 (subst (make-mul 'x 3) 'x 4)
 (make-mul 4 3))

(check-expect
 (subst (make-add 'x 3) 'x 4)
 (make-add 4 3))

(check-expect
 (subst (make-add 'x 3) 'y 4)
 (make-add 'x 3))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(mul? ex) (make-mul
                (subst (mul-left ex) x v)
                (subst (mul-right ex) x v))]
    [(add? ex) (make-add
                (subst (add-left ex) x v)
                (subst (add-right ex) x v))]))

; BSL-var-expr -> Boolean
; is bsl-v-exp a bsl-exp?

(check-expect
 (numeric? 5)
 #t)
(check-expect
 (numeric? (make-mul 3 5))
 #t)
(check-expect
 (numeric? (make-add 3 5))
 #t)
(check-expect
 (numeric? (make-add (make-mul 3 4) 5))
 #t)
(check-expect
 (numeric? (make-add (make-mul 's 4) 5))
 #f)

(define (numeric? bsl-v-exp)
  (cond
    [(number? bsl-v-exp) #t]
    [(mul? bsl-v-exp)
     (and (numeric? (mul-left bsl-v-exp))
          (numeric? (mul-right bsl-v-exp)))]
    [(add? bsl-v-exp)
     (and (numeric? (add-left bsl-v-exp))
          (numeric? (add-right bsl-v-exp)))]
    [else #f]))

; BSL-func-expr Symbol Symbol BSL-func-expr -> BSL-expr
; eval a function containing expression

(check-expect
 (eval-definition1
  (make-func 'k (make-add 1 1))
  'k
  'x
  (mul 'x (make-add 1 2)))
 6)

(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error "non-declared variable")]
    [(and
      (func? ex) (symbol=? f (func-name ex)))
     (subst b x (eval-definiton1 (func-expr ex) f x b))]
    [else ))






















