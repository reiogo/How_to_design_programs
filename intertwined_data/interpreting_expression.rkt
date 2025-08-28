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
      (eval-expression (mul-right b-exp)))]
    [(func? b-exp) (error "shouldn't be func")]
    ))

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


(define (atom? x)
  (cond
    [(or
      (number? x)
      (string? x)
      (symbol? x)) #t]
    [else #f]))




(define WRONG "wrong kind of S-expression") ; the error message

; S-expr -> BSL-var-expr
; creates representation of a BSL expression for s (if possible)

(check-expect
 (parse '(+ 5 4)) (make-add 5 4))

(check-expect
 (parse '(* 5 4)) (make-mul 5 4))

(check-error
 (parse '(+ 5)) WRONG)

(check-error
 (parse '(/ 5 3)) WRONG)

(check-error
 (parse '(* 3 5 3)) WRONG)

(check-expect
 (parse '(+ d 3)) (make-add 'd 3))

(check-error
 (parse '(+ "d" 3)) "strings not allowed")



(define (parse s)
  (local (; S-expr -> BSL-var-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))

          ; SL -> BSL-var-expr
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(< L 3)
                 (error WRONG)]
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [else
                 (error WRONG)])))
          ; Atom -> BSL-var-expr
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "strings not allowed")]
              [(symbol? s) s])))
    (parse s)))


; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

;x 'x
; (+ x 3)
; (make-add 'x 3)

; (* 1/2 (* x 3))
; (make-mul 1/2 (make-mul 'x 3))

; (+ (* x x)
; (* y y))

; make-add (make-mul 'x 'x)
; (make-mul 'y 'y))

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
                (subst (add-right ex) x v))]
    [(func? ex)
     (make-func
      (func-name ex)
      (subst (func-expr ex) x v))]))

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

; BSL-var-expr -> Number
; if numeric? then evaluate

(check-expect
 (eval-variable (make-mul 3 4)) 12)
(check-expect
 (eval-variable (make-add 3 4)) 7)
(check-error
 (eval-variable (make-mul 3 's)) "impossible to eval")


(define (eval-variable bve)
  (if
   (numeric? bve)
   (eval-expression bve)
   (error "impossible to eval")))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
; (cons Symbol (cons Number '())).
'((x 3)
  (y 4))

; BSL-var-expr AL -> BSL-expr
; subs in all the variables from da and applies it to ex
; then evals ex

(check-expect
 (eval-variable*
  (make-mul (make-add 'y 4) 'x)
  '((x 3)
    (y 4)))
 24)
(check-error
 (eval-variable*
  (make-mul (make-add 'y 4) 'z)
  '((x 3)
    (y 4)))
 "impossible to eval")

(define (eval-variable* ex da)
  (cond
    [(empty? da) (eval-variable ex)]
    [else
     (eval-variable*
      (subst ex (first (first da)) (second (first da)))
      (rest da))]))

; AL Symbol -> Number
; look up val of key

(check-expect
 (lookup-con '((x 3) (y 4)) 'y) 4)
(check-error
 (lookup-con '((x 3) (y 4)) 'z) "no key")

(define (lookup-con da x)
  (cond
    [(empty? da) (error "no key")]
    [else
     (if
      (symbol=? (first (first da)) x)
      (second (first da))
      (lookup-con (rest da) x))]))

; BSL-var-expr AL -> BSL-expr
; subs in all the variables from da and applies it to ex
; then evals ex

(check-expect
 (eval-var-lookup
  (make-mul (make-add 'y 4) 'x)
  '((x 3)
    (y 4)))
 24)
(check-error
 (eval-var-lookup
  (make-mul (make-add 'y 4) 'z)
  '((x 3)
    (y 4)))
 "no key")

(define (eval-var-lookup bve da)
  (local (; BSL-var-expr AL -> BSL-expr
          (define (make-exp bve da)
            (cond
              [(number? bve) bve]
              [(symbol? bve) (lookup-con da bve)]
              [(add? bve)
               (make-add
                (make-exp (add-left bve) da)
                (make-exp (add-right bve) da))]
              [(mul? bve)
               (make-mul
                (make-exp (mul-left bve) da)
                (make-exp (mul-right bve) da))])))
    (eval-variable (make-exp bve da))))

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


; BSL-func-expr Symbol Symbol BSL-func-expr -> BSL-expr
; eval a expression containing a function

(check-expect
 (eval-definition1
  (make-func 'k (make-add 1 1))
  'k
  'x
  (make-mul 'x (make-add 1 2)))
 6)

(check-error
 (eval-definition1
  (make-mul (make-func 'k (make-add 1 1)) 'y)
  (func-name (make-func 'k (make-add 1 1)))
  'x
  (make-mul 'x (make-add 1 2)))
 "non-declared variable")

(define (eval-definition1 ex f x b)
  (local (
          (define (make-exp ex f x b)
            (cond
              [(number? ex) ex]
              [(symbol? ex) (error "non-declared variable")]
              [(mul? ex)
               (make-mul
                (make-exp (mul-left ex) f x b)
                (make-exp (mul-right ex) f x b))]
              [(add? ex)
               (make-add
                (make-exp (add-left ex) f x b)
                (make-exp (add-right ex) f x b))]
              [(and
                (func? ex) (symbol=? f (func-name ex)))
               (make-exp (subst b x (make-exp (func-expr ex) f x b))
                         f x b)])))
    (eval-expression (make-exp ex f x b))))


; bsl-func-def* is [List-of BSL-func-def]

(define-struct bsl-func-def [name parameter body])
; a BSL-func-def is a structure
; (make-bsl-func-def Symbol Symbol BSL-func-expr)
; interpretation (make-bsl-func-def n p exp)
; n is the name, p is the parameter, exp is the function body
(define f (make-bsl-func-def 'f 'x (make-add 3 'x)))
(define g (make-bsl-func-def 'g 'y (make-func 'f (make-mul 2 'y))))
(define h  (make-bsl-func-def 'h 'v
                       (make-add
                        (make-func 'f 'v) (make-func 'g 'v))))
(define da-fgh
  (list
   (make-bsl-func-def 'f 'x (make-add 3 'x))
   (make-bsl-func-def 'g 'y (make-func 'f (make-mul 2 'y)))
   (make-bsl-func-def 'h 'v
                       (make-add
                        (make-func 'f 'v) (make-func 'g 'v)))))


 ; BSL-func-def* Symbol -> BSL-func-def
; retrieves the definition of f in da
; or signal "undefined function" if da does not contain one

(check-expect (lookup-def da-fgh 'f) f)
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'x) "undefined function")

(define (lookup-def da f)
  (cond
    [(empty? da) (error "undefined function")]
    [else
     (if
      (symbol=? f (bsl-func-def-name (first da)))
      (first da)
      (lookup-def (rest da) f))]))

; BSL-func-expr BSL-func-def* -> BSL-expr
; evaluates an expression with functions

(check-expect
 (eval-function*
  (make-func 'g (make-mul 3 4))
  da-fgh)
 27)


(define (eval-function* bfe bfd*)
  (local (
          (define (make-exp bfe bfd*)
            (cond
              [(number? bfe) bfe]
              [(symbol? bfe) (error "non-declared variable")]
              [(mul? bfe)
               (make-mul
                (make-exp (mul-left bfe) bfd*)
                (make-exp (mul-right bfe) bfd*))]
              [(add? bfe)
               (make-add
                (make-exp (add-left bfe) bfd*)
                (make-exp (add-right bfe) bfd*))]
              [(func? bfe)
               (local ((define spec-func (lookup-def bfd* (func-name bfe))))
               (make-exp
                (subst bfe (bsl-func-def-parameter spec-func)
                       (make-exp (func-expr bfe) bfd*))
                         bfd*))])))
    (eval-expression (make-exp bfe bfd*))))














