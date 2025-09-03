;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expressions) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; S-Expression

; An S-expr (short for S-expression) is one of:
; - Atom
; - SL

; An SL (short for S-list) is one of:
; - '()
; - (cons S-expr SL)

; An Atom is one of:
; - Number
; - String
; - Symbol

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



; S-exp Symbol -> Number
; how many times does sy appear in s-exp

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

; S-expr Symbol -> N
; counts all occurences of sy in sexp
(define (count-o s-exp sy)
  (cond
    [(atom? s-exp) (count-atom s-exp sy)]
    [else (count-sl s-exp sy)]))

; SL Symbol -> N
; counts all occurences of sy in sl
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+
      (count (first sl) sy)
      (count-sl (rest sl) sy))]))

; Atom Symbol -> N
; counts all occurences of sy in at
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))


; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(define (count sexp sy)
  (local (; S-expr Symbol -> N
          ; the main function
          (define (count-sexp sexp)
            (cond
              [(atom? sexp) (count-atom sexp)]
              [else (count-sl sexp)]))
          ; SL Symbol -> N
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count-sexp (first sl))
                       (count-sl (rest sl)))]))
          ; Atom Symbol -> N
          ; counts all occurrences of sy in at
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    ; — IN —
    (count-sexp sexp)))


; S-expr -> Number
; when an atom has a depth of 1, find the depth of s-exp

(check-expect (depth 'hello) 1)
(check-expect (depth '(hello world)) 2)
(check-expect (depth '(((hello) world) 3)) 4)
(check-expect (depth '(((hello turn (world form)) world 3) hi 3)) 5)

(define (depth s-exp)
  (local (; S-expr -> Number
          ; count depth of s-exp
          (define (depth-s-exp s-exp)
            (cond
              [(atom? s-exp) (depth-atom s-exp)]
              [else (depth-sl s-exp)]))
          ; SL -> Number
          ; count depth of sl
          (define (depth-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (max
                (+ 1 (depth-s-exp (first sl)))
                (depth-sl (rest sl)))]))
          ; Atom -> Number
          ; count depth of atom
          (define (depth-atom at)
            (cond
              [(number? at) 1]
              [(string? at) 1]
              [(symbol? at) 1]
              ))
          )
    (depth-s-exp s-exp)))

; S-expr Symbol Symbol -> S-expr
; replaces old with new in s

(check-expect (substitute '( hi "hello" 3) 'hi 'hello)
              '(hello "hello" 3))

(check-expect (substitute '( hi "hello" (hi) 3) 'hi 'hello)
              '(hello "hello" (hello) 3))

(define (substitute s-exp old new)
  (local (; S-expr -> S-expr
          ; replace old with new in s0
          (define (sub-s-exp s0)
            (cond
              [(atom? s0) (sub-atom s0)]
              [else (sub-sl s0)]))
          ; SL -> SL
          ; replace old with new in s0
          (define (sub-sl sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons
                (sub-s-exp (first sl))
                (sub-sl (rest sl)))]))
          ; Atom -> Atom
          ; replace at if old with new
          (define (sub-atom at)
            (cond
              [(string? at) at]
              [(number? at) at]
              [(symbol? at) (if (symbol=? at old) new at)])
          ))
    (sub-s-exp s-exp)))


; An S-expr (short for S-expression) is one of:
; - Number
; - String
; - Symbol
; - [List-of S-expr]

; S-expr Symbol -> N
; count all occurences of sy in s-exp

(check-expect (count-abbr 'world 'hello) 0)
(check-expect (count-abbr '(world hello) 'hello) 1)
(check-expect (count-abbr '(((world) hello) hello) 'hello) 2)

(define (count-abbr s-exp sy)
  (cond
    [(number? s-exp) 0]
    [(string? s-exp) 0]
    [(symbol? s-exp) (if
                      (symbol=? s-exp sy)
                      1
                      0)]
    [(list? s-exp) (cond
                     [(empty? s-exp) 0]
                     [else
                      (+
                       (count-abbr (first s-exp) sy)
                       (count-abbr (rest s-exp) sy))])]))

; An [S-expr X] (short for S-expression) is one of:
; - X
; - [SL X]

; An [SL X] (short for S-list) is one of:
; - '()
; - (cons [S-expr X] [SL X])
























