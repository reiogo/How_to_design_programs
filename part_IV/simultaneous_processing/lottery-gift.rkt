;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lottery-gift) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; [List-of String] -> [List-of String]
; picks a "random" non-identity arrangement of names
(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))


; ARRANGEMENT ======================================================

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of the given list of names

(check-expect
 (arrangements
  (list "w" "h"))
 (list 
  (list "w" "h")
  (list "h" "w")))

(check-expect
 (arrangements (explode "hi"))
 (list
  (list "h" "i")
  (list "i" "h")))

(check-expect
 (arrangements '())
 (list '()))
(check-expect
 (arrangements '("jane" "louise"))
 '(("jane" "louise")
   ("louise" "jane")))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else
     (insert-everywhere/in-all-words (first w)
                                     (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
; insert character into every position of all words in alow

(check-expect
 (insert-everywhere/in-all-words
  "d"
  (cons (list "e" "r")
        (cons (list "r" "e") '())))
 (list
  (list "d" "e" "r")
  (list "e" "d" "r")
  (list "e" "r" "d")
  (list "d" "r" "e")
  (list "r" "d" "e")
  (list "r" "e" "d")))

(check-expect
 (insert-everywhere/in-all-words
  "d" '())
 '())

(check-expect
 (insert-everywhere/in-all-words
  "d" (list (list "h")))
 (list
  (list "d" "h")
  (list "h" "d")))

(define (insert-everywhere/in-all-words s alow)
  (cond
    [(empty? alow) '()]
    [else
     (append
      (insert-everywhere s (first alow))
      (insert-everywhere/in-all-words s (rest alow)))]))
; 1String Word -> List-of-words
; insert s into every position of aw

(check-expect
 (insert-everywhere "e" (list "s" "h" "i"))
 (list
  (list "e" "s" "h" "i")
  (list "s" "e" "h" "i")
  (list "s" "h" "e" "i")
  (list "s" "h" "i" "e")))

(check-expect
 (insert-everywhere "s" (list "h" "i"))
 (list
  (list "s" "h" "i")
  (list "h" "s" "i")
  (list "h" "i" "s")))

(check-expect
 (insert-everywhere "s" (list "i"))
 (list
  (list "s" "i")
  (list "i" "s")))

(check-expect
 (insert-everywhere "s" '())
 (list (list "s")))

(define (insert-everywhere s aw)
  (cond
    [(empty? aw) (list (list s))]
    [else
     (cons
      (cons s aw)
      (append-all (first aw)
                  (insert-everywhere s (rest aw))))]
    ))

; 1String List-of-words -> List-of-words
; Add s to the start of every word in alow

(check-expect
 (append-all "s" (list (list "a" "b") (list "c" "d")))
 (list
  (list "s" "a" "b")
  (list "s" "c" "d")))

(check-expect
 (append-all "s" (list (list "w")))
 (list (list "s" "w")))

(check-expect
 (append-all "s" '())
 '())

(define (append-all s alow)
  (cond
    [(empty? alow) '()]
    [else
     (cons
      (cons s (first alow))
      (append-all s (rest alow)))]
    ))
; ======================================================


; [List-of X] -> X
; returns the ith item from list

(check-expect (pick '(1 2 3) 2) 3)
(check-error (pick '() 2) "given list too short")

(define (pick lox n)
  (cond
    [(empty? lox) (error "given list too short")]
    [(= n 0) (first lox)]
    [else
     (pick (rest lox) (sub1 n))]))

; [List-of X] -> X
; returns a random item from the list
; assume the list is not empty

(check-random
 (random-pick '(1 2 3)) (pick '(1 2 3) (random 3)))
               
(define (random-pick l)
  (pick l (random (length l))))

; [List-of String] [List-of [List-of String]]
; ->
; [List-of [List-of String]]
; produces the list of those lists in ll that do not agree
; with names at any place

(check-expect
 (non-same '("h" "i") '(("h" "i") ("i" "h")))
 '(("i" "h")))
(check-expect
 (non-same '("h" "i" "j")
           (list
            (list "h" "i" "j")
            (list "i" "h" "j")
            (list "i" "j" "h")
            (list "h" "j" "i")
            (list "j" "h" "i")
            (list "j" "i" "h")))
 (list
  (list "i" "j" "h")
  (list "j" "h" "i")))

(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [else
     (local ((define (agreements? names lon)
               (cond
                 [(empty? lon) #f]
                 [else
                  (if
                   (string=? (first names) (first lon))
                   #t
                   (agreements? (rest names) (rest lon)))])))
       (if
        (agreements? names (first ll))
        (non-same names (rest ll))
        (cons (first ll) (non-same names (rest ll)))
        ))]))

