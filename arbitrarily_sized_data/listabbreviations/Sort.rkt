;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Sort) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; SORT

;List-of-numbers -> List-of-numbers
; rearrange alon in descending order

(check-expect (sort> '())'())

(check-expect (sort> (list 12 20 -5)) (list 20 12 -5))

(check-expect (sort> (list 3 2 1)) (list 3 2 1))

(check-expect (sort> (list 1 2 3)) (list 3 2 1))

(check-satisfied (sort> (list 1 2 3)) sorted>?)
 
(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [else
     (insert (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; insert n into the sorted list of numbers alon

(check-expect (insert 5 '()) (list 5))

(check-expect (insert 5 (list 6)) (list 6 5))

(check-expect (insert 5 (list 4)) (list 5 4))

(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(check-expect (insert -3 (list 30 9 2 1)) (list 30 9 2 1 -3))

(check-expect (insert 40 (list 30 9 2 1)) (list 40 30 9 2 1))

(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else
     (if
      (>= n (first alon))
      (cons n alon)
      (cons (first alon) (insert n (rest alon))))]))

; SORTED>?
; NEList-of-temperatures -> Boolean
; determines whether every temperature is
; strictly more than the next number

(check-expect
 (sorted>?
  (cons 1
        (cons 2 '()))) #false)

(check-expect
 (sorted>?
  (cons 3
        (cons 2 '()))) #true)

(check-expect
 (sorted>?
  (cons 0
        (cons 3
              (cons 2 '())))) #false)

(define (sorted>? anelot)
  (cond
    [(empty? (rest anelot)) #true]
    [(cons? (rest anelot))
     (and
      (>
       (first anelot)
       (first (rest anelot)))
      (sorted>? (rest anelot)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of 1

;(check-expect (sort>/bad (list 1 5 3)) (list 5 3 1))

; (check-satisfied (sort>/bad (list 1 5 3)) sorted>?)

(define (sort>/bad l)
  '(9 8 7 6 5 4 3 2 1 0))


(define-struct email [from date message])
; An Email Message is a structure:
; (make-email String Number String)
; interpretation (make-email f d m) represents text m sent by
; f, d seconds after the beginning of time

; SORT-EMAIL
; List-of-emails -> List-of-emails
; sort email by date in descending order

(check-expect
 (sort-email '()) '())

(check-expect
 (sort-email
  (list
   (make-email "a" 10 "hi")
   (make-email "b" 12 "hello")
   (make-email "c" 14 "hello")))
 (list
  (make-email "c" 14 "hello")
  (make-email "b" 12 "hello")
  (make-email "a" 10 "hi")))
                           
(define (sort-email aloe)
  (cond
    [(empty? aloe) '()]
    [else
     (ins-email
      (first aloe)
      (sort-email (rest aloe)))]))

; Email Loe -> Loe
; Insert email into the correct position of a sorted loe

(check-expect
 (ins-email
  (make-email "a" 10 "hi")
  (list
   (make-email "c" 14 "hello")
   (make-email "b" 12 "hello")))
 (list
  (make-email "c" 14 "hello")
  (make-email "b" 12 "hello")
  (make-email "a" 10 "hi")))

(check-expect
 (ins-email
  (make-email "a" 16 "hi")
  (list
   (make-email "c" 14 "hello")
   (make-email "b" 12 "hello")))
 (list
  (make-email "a" 16 "hi")
  (make-email "c" 14 "hello")
  (make-email "b" 12 "hello")))

(check-expect
 (ins-email
  (make-email "a" 10 "hi")
  '())
 (list (make-email "a" 10 "hi")))
 
(define (ins-email e aloe)
  (cond
    [(empty? aloe) (list e)]
    [else
     (if (> (email-date e) (email-date (first aloe)))
         (cons e aloe)
         (cons (first aloe) (ins-email e (rest aloe))))]))

; Loe -> Loe
; Sort a list of email lexicographically

(check-expect (sort-email-lex '()) '())

(check-expect
 (sort-email-lex
  (list
   (make-email "b" 12 "hello")
   (make-email "a" 10 "hi")
   (make-email "c" 14 "hello")))
 (list
  (make-email "c" 14 "hello")
  (make-email "b" 12 "hello")
  (make-email "a" 10 "hi")))

(define (sort-email-lex aloe)
  (cond
    [(empty? aloe) '()]
    [else
     (ins-email-lex
      (first aloe)
      (sort-email-lex (rest aloe))
      )]))


; Email Loe -> Loe
; Insert an email by name into the correct place of a sorted loe

(check-expect
 (ins-email-lex
  (make-email "c" 13  "hello")
  '())
 (list
  (make-email "c" 13  "hello")))

(check-expect
 (ins-email-lex
  (make-email "c" 14 "hello")
  (list
   (make-email "b" 12 "hello")
   (make-email "a" 10 "hi")))
 (list
  (make-email "c" 14 "hello")
  (make-email "b" 12 "hello")
  (make-email "a" 10 "hi")))
  
  
(define (ins-email-lex e aloe)
  (cond
    [(empty? aloe) (list e)]
    [else
     (if
      (string<?  (email-from (first aloe)) (email-from e))
      (cons e aloe)
      (cons
       (first aloe)
       (ins-email-lex e (rest aloe))))]))


(define-struct gp [name score])
; A GamePlayer is a structure:
; (make-gp String Number)
; interpretation (make-gp p s) represents player p who scored
; a maximum of s points

; logp -> logp
; sort a list of gameplayers by score in descending order

(check-expect
 (sort-gp> '()) '())

(check-expect
 (sort-gp>
  (list
   (make-gp "a" 4)
   (make-gp "b" 2)
   (make-gp "c" 5)))
 (list
  (make-gp "c" 5)
  (make-gp "a" 4)
  (make-gp "b" 2)))
   
(define (sort-gp> alogp)
  (cond
    [(empty? alogp) '()]
    [else
     (ins-gp
      (first alogp)
      (sort-gp> (rest alogp)))]))

; GP Logp -> Logp
; Insert gp into the right place of sorted logp

(check-expect
 (ins-gp
  (make-gp "a" 4)
  (list
   (make-gp "a" 4)
   (make-gp "b" 2)
   (make-gp "c" 5)))
 (list
  (make-gp "a" 4)
  (make-gp "a" 4)
  (make-gp "b" 2)
  (make-gp "c" 5)
  ))
              
(define (ins-gp gp alogp)
  (cond
    [(empty? alogp) (list gp)]
    [else
     (if (> (gp-score gp) (gp-score (first alogp)))
         (cons gp alogp)
         (cons
          (first alogp)
          (ins-gp gp (rest alogp))))]))

; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else
     (or (= (first alon) n)
         (search n (rest alon)))]))


; Number List-of-Numbers -> Boolean
; Search for a number in a sorted list

(check-expect
 (search-sorted
  5
  (list 1 2 3 4))
 #false)

(check-expect
 (search-sorted
  5
  (list 1 2 3 4 5))
 #true)

(check-expect
 (search-sorted
  5
  (list 1 2 3 5 43 80 93))
 #true)
(check-expect
 (search-sorted
  5
  (list 1 2 3 43 80 93))
 #false)

(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [else
     (cond
       [(= n (list-ref alon (middle alon))) #true]
       [(> n (list-ref alon (middle alon)))
        (search-sorted n (after (middle alon) alon))]
       [else
        (search-sorted n (before (middle alon) alon))])
     ]))


; Lon -> Number
; find the middle index of a list
; cannot handle an empty list

(check-expect
 (middle
  (list 1 2 3 4)) 1)

(check-expect
 (middle
  (list 1 2 3 4 5)) 2)
               
(define (middle alon)
  (- (ceiling (/ (length alon) 2)) 1))

; Number Lon -> Lon
; return a list up until a certain index (0-index)

(check-expect
 (before 3
         (list 1 2 3 4 5 6))
 (list 1 2 3 ))

(check-expect
 (before 2
         (list 1 2 3 4 5 6))
 (list 1 2 ))
(check-expect
 (before 0
         (list 1 2 3 4 5 6))
 '())

(define (before n alon)
  (cond
    [(zero? n) '()]
    [else
     (cons
      (first alon)
      (before (sub1 n) (rest alon)))]))

; Number Lon -> Lon
; return a list after a certain index (0-index)

(check-expect
 (after
  1
  (list 1 2 3 4 5 6))
 (list 3 4 5 6))

(check-expect
 (after
  5
  (list 1 2 3 4 5 6))
 '())

(check-expect
 (after
  4
  (list 1 2 3 4 5 6))
 (list 6))

(define (after n alon)
  (if
   (zero? n)
   (rest alon)
   (after (sub1 n) (rest alon))))


; Number Lon -> Boolean
; search for a number in a sorted list

(check-expect
 (not-stupid
  5
  (list 1 2 3 4))
 #false)

(check-expect
 (not-stupid
  5
  (list 1 2 3 4 5))
 #true)

(check-expect
 (not-stupid
  5
  (list 1 2 3 5 43 80 93))
 #true)
(check-expect
 (not-stupid
  5
  (list 1 2 3 43 80 93))
 
 #false)

(define (not-stupid n alon)
  (cond
    [(empty? alon) #false]
    [else
     (cond
       [(= (first alon) n) #true]
       [(> (first alon) n) #false]
       [else (not-stupid n (rest alon))])]))


; lo1s -> llo1s
; list of all the prefixes of a list of 1strings

(check-expect
 (prefixes
  (list "a" "b" "c"))
 (list
  (list "a")
  (list "a" "b")
  (list "a" "b" "c")))
       

(define (prefixes alo1s)
  (cond
    [(empty? alo1s) '()]
    [else
     (cons
      (list (first alo1s))
      (ins-all 
       (first alo1s)
       (prefixes (rest alo1s))))]))

; 1String llo1s -> llos
; Insert a 1String into a llo1s

(check-expect
 (ins-all
  "b"
  '())
 '())

(check-expect
 (ins-all
  "b"
  (list (list "c")))
 (list (list "b" "c")))

(check-expect
 (ins-all
  "a"
  (list
   (list "b")
   (list "b" "c")))
 (list
  (list "a" "b")
  (list "a" "b" "c")))

(define (ins-all s allo1s)
  (cond
    [(empty? allo1s) '()]
    [else
     (cons
      (cons s (first allo1s))
      (ins-all s (rest allo1s)))]))


; lo1s -> llo1s
; Produces a list of suffixes for a given list of 1strings

(check-expect
 (suffixes
  (list "a" "b" "c"))
 (list
  (list "c")
  (list "b" "c")
  (list "a" "b" "c")))

(check-expect
 (suffixes
  (list "a" "b"))
 (list
  (list "b")
  (list "a" "b")))

(check-expect
 (suffixes
  (list "a"))
 (list
  (list "a")))

(check-expect
 (suffixes
  '())
 '())

(define (suffixes alo1s)
  (reverse (reverse-suffixes alo1s)))


(define (reverse-suffixes alo1s)
    (cond
    [(empty? alo1s) '()]
    [else
     (cons
      alo1s
      (reverse-suffixes (rest alo1s)))]))

;(define (suffixes alo1s)
 ; (cond
  ;  [(empty? alo1s) '()]
   ; [else
    ; (cons (list (last alo1s))
     ;; (ins-end
       ;(last alo1s)
      ; (suffixes (front alo1s)))]))



























