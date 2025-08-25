;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simplifying) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Simplifying functions

; X -> Boolean
; Is x a number, string, or symbol?

(check-expect (atom? 3) #t)
(check-expect (atom? "hi") #t)
(check-expect (atom? 'e) #t)
(check-expect (atom? #true) #f)

(define (atom? x)
  (cond
    [(or
      (number? x)
      (string? x)
      (symbol? x)) #t]
    [else #f]))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in s-exp with new
(check-expect (substitute 'world 'hello 0) 'world)
(check-expect (substitute '(world hello) 'hello 'bye) '(world bye))
(check-expect
 (substitute '(((world) bye) bye) 'bye '42) '(((world) 42) 42))

(define (substitute s-exp old new)
  (cond
    [(atom? s-exp) (if (eq? s-exp old) new s-exp)]
    [else (map (lambda (s) (substitute s old new)) s-exp)]))
























