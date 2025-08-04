;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-vs-set) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; List-of-string String -> N
; determine how often s occurs in los
(check-expect (count '() "hi") 0)
(check-expect (count (cons "hi" '()) "hi") 1)
(check-expect (count (cons "hi" '()) "he") 0)
(check-expect (count (cons "he" (cons "hi" '())) "hi") 1)
(check-expect (count (cons "ih" (cons "hi" '())) "he") 0)
(define (count los s)
  (cond
    [(empty? los) 0]
    [(cons? los)
     (if (string=? (first los) s)
         (add1 (count (rest los) s))
         (count (rest los) s))]))


; A Son.L is one of:
; - empty
; - (cons number Son.L)
;
; Son is used when it applies
; to both Son.L and Son.R

; A Son.R is one of:
; - empty
; (cons Numver Son.R)
;
; Constraint If s is a Son.R,
; no number occurs twice in s.


; Son
(define es '())

; Number Son -> Son
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; remove x from s
(define s1.L
  (cons 1 (cons 1 '())))
(check-expect (set-.L 1 s1.L) es)

(define (set-.L x s)
  (remove-all x s))

; Number Son.R -> Son.R
; remove x from s
(define s1.R
  (cons 1 '()))

(check-expect (set-.R 1 s1.R) es)

(define (set-.R x s)
  (remove x s))

; Number Son -> Son
; subtract x from s
(define (set- x s)
  s)

; Son -> Boolean
; #true if 1 is a member s; #false otherwise
(define (not-member-1? s)
  (not (in? 1 s)))

; (check-satified (set- 1 set123) not-member-1?)

; Son -> Boolean
; #true if 3 is a member of s; false otherwise
(define (member-3? s)
  (in? 3 s))

; SET+ L
; Set.L Number -> Set.L
; Add n to Set.L
(check-satisfied (Set+.L (cons 2 '()) 3) member-3?)
(check-expect (Set+.L (cons 2 (cons 3 '())) 3)
              (cons 3 (cons 2 (cons 3 '()))))
(define (Set+.L s n)
  (cons n s))


; SET+ R
; Set.R Number -> Set.R
; Add n to Set.R
(check-satisfied (Set+.R (cons 2 '()) 3) member-3?)
(check-expect (Set+.R (cons 2 (cons 3 '())) 3)
              (cons 2 (cons 3 '())))
(define (Set+.R s n)
  (if (member? n s)
  s
  (cons n s)))

















