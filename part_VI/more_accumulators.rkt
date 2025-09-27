;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname more_accumulators) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; More Uses of Accumulation

; A Lam is one of:
; - a Symbol
; - (list 'l (list Symbol) Lam)
; - (list Lam Lam)

; A Lam.v2 is one of:
; - a Symbol
; - Lambda
; - App

(define-struct mylam [para body])
; A Lambda is a structure:
; (make-mylam [List-of Symbol] Lam.v2)
; interpretation: (make-mylam p b) p is the list of
; parameters, b is the body of the expression

(define-struct app [args func])
; A App (short for application) is a structure
; (make-app Lam Lam)
; interpretation: (make-app a b) a is the argument expression
; b is the body expression

(define ex1 '(l (x) x))
(define ex2 '(l (x) y))
(define ex3 '(l (y) (l (x) y)))
(define ex4 '((l (x) (x x)) (l (x) (x x))))
(define ex5 '(((l (y) (l (x) y)) (l (z) z)) (l (w) w)))
(define ex6 '((l (x) x) (l (y) x)))


(define ex1.v2 (make-mylam 'x 'x))
(define ex2.v2 (make-mylam 'y 'x))
(define ex3.v2 (make-mylam 'y (make-mylam 'x 'y)))

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

(check-expect (is-l? ex1) #t)

(define (is-l? x)
  (and
   (and (list? x) (= (length x) 3))
   (symbol=? (first x) 'l)
   (and (list? (second x)) (andmap (lambda (s) (symbol? s)) (second x)))
   (is-lam? (third x))))

; X -> Boolean
; is it l?

(check-expect (is-app? ex4) #t)

(define (is-app? x)
  (and
   (and (list? x) (= (length x) 2))
   (is-lam? (first x))
   (is-lam? (second x))))

; [X -> Y] -> Symbol
; extract param from a lambda expression

(check-expect (l-para ex1) 'x)

(define (l-para x)
  (first (second x)))

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

(check-expect (declareds ex1) '(x))

(define (declareds x)
  (cons
   (l-para x)
   (declared-in-expr (l-body x))))

; Lam -> [List-of Symbol]
; list of all symbols used as lambda parameters in a expression

(check-expect (declared-in-expr ex4) '(x x))

(define (declared-in-expr x)
  (cond
    [(is-var? x) '()]
    [(is-l? x) (declareds x)]
    [(is-app? x)
     (append
      (declared-in-expr (app-fun x))
      (declared-in-expr (app-arg x)))]))

; Lam -> Lam
; replace all symbols s in le0 with '*undeclared if they
; do not occur within the body of a lambda whose parameter
; is s

(check-expect (undeclareds ex1) (list 'l (list 'x) '*declared:x))
(check-expect (undeclareds ex2) '(l (x) *undeclared:y))
(check-expect (undeclareds ex3) (list
                                 'l
                                 (list 'y)
                                 (list 'l (list 'x) '*declared:y)))
(check-expect (undeclareds ex4) (list
                                 (list
                                  'l
                                  (list 'x)
                                  (list '*declared:x '*declared:x))
                                 (list
                                  'l
                                  (list 'x)
                                  (list '*declared:x '*declared:x))))
(check-expect (undeclareds ex6)
              '((l (x) *declared:x) (l (y) *undeclared:x)))
(check-expect (undeclareds
               '(l (*undeclared) ((l (x) (x *undeclared)) y)))
              (list
               'l
               (list '*undeclared)
               (list
                (list
                 'l
                 (list 'x)
                 (list
                  '*declared:x
                  '*declared:*undeclared))
                '*undeclared:y)))

(define (undeclareds le0)
  (local (; Lam ??? -> Lam
          ; accumulator: declareds represents the list of lambda
          ; parameters encountered on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (local (; String Symbol -> Symbol
                       (define (convert s0 s1)
                         (string->symbol
                          (string-append s0 (symbol->string s1)))))
                 (if (member? le declareds)
                     (convert "*declared:" le)
                     (convert "*undeclared:" le)))]
              [(is-l? le)
               (local ((define para (l-para le))
                       (define newdec (cons para declareds))
                       (define body (undeclareds/a (l-body le) newdec)))
                 (list 'l (list para) body))]
              [(is-app? le)
               (list (undeclareds/a (app-arg le) declareds)
                     (undeclareds/a (app-fun le) declareds))])))
    (undeclareds/a le0 '())))

(check-expect (undeclareds.v2 ex1.v2)
              (make-mylam 'x '*declared:x))
(check-expect (undeclareds.v2 ex2.v2)
              (make-mylam 'y '*undeclared:x))
(check-expect (undeclareds.v2 ex3.v2)
              (make-mylam 'y (make-mylam 'x '*declared:y)))

(define (undeclareds.v2 le0)
  (local (; Lam ??? -> Lam
          ; accumulator: declareds represents the list of lambda
          ; parameters encountered on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (local (; String Symbol -> Symbol
                       (define (convert s0 s1)
                         (string->symbol
                          (string-append s0 (symbol->string s1)))))
                 (if (member? le declareds)
                     (convert "*declared:" le)
                     (convert "*undeclared:" le)))]
              [(mylam? le)
               (local ((define para (mylam-para le))
                       (define newdec (cons para declareds))
                       (define body (undeclareds/a
                                     (mylam-body le) newdec)))
                 (make-mylam para body))]
              [(app? le)
               (make-app (undeclareds/a (app-args le) declareds)
                         (undeclareds/a (app-func le) declareds))])))
    (undeclareds/a le0 '())))


; Lam -> Lam
; replaces all variables with a natural number that
; represents how far away the declaration is. ( 0 is the first lambda
; on its path, 1 is the second etc)

(check-expect (static-distance ex1) '(l (x) 0))
(check-expect (static-distance ex2) '(l (x) y))
(check-expect (static-distance ex3) '(l (y) (l (x) 1)))
(check-expect (static-distance ex4)
              '((l (x) (0 0)) (l (x) (0 0))))
(check-expect (static-distance ex6)
              '((l (x) 0) (l (y) x)))
(check-expect (static-distance
               '(l (*undeclared) ((l (x) (x *undeclared)) y)))
              '(l (*undeclared) ((l (x) (0 1)) y)))
(check-expect (static-distance
               '((l (x) ((l (y) (y x)) x)) (l (z) z)))
              '((l (x) ((l (y) (0 1)) 0)) (l (z) 0)))

(define (static-distance le0)
  (local (; Lam ??? -> Lam
          ; accumulator: declareds represents the list of lambda
          ; parameters encountered on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (local (; [List-of Symbol] Symbol Number -> Number
                       ; count how many till the s in alos
                       (define (count alos s n)
                         (cond
                           [(symbol=? (first alos) s) n]
                           [else (count (rest alos) s (add1 n))])))
                 (if (member? le declareds)
                     (count declareds le 0) le))]
              [(is-l? le)
               (local ((define para (l-para le))
                       (define newdec (cons para declareds))
                       (define body (undeclareds/a (l-body le) newdec)))
                 (list 'l (list para) body))]
              [(is-app? le)
               (list (undeclareds/a (app-arg le) declareds)
                     (undeclareds/a (app-fun le) declareds))])))
    (undeclareds/a le0 '())))














