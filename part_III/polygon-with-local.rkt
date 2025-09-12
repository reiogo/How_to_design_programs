;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname polygon-with-local) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; POLYGON with local

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
(define MT (empty-scene 50 50))

; Polygon -> Image
; adds an image of p to MT
(define (render-polygon p)
  (render-line (connect-dots p) (first p) (last p)))

; A NELoP is one of:
; – (cons Posn '())
; – (cons Posn NELoP)

; NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots p)
  (cond
    [(empty? (rest p)) MT]
    [else
     (render-line
      (connect-dots (rest p)) (first p) (second p))]))

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Polygon -> Image
; adds an image of p to MT
(define (render-polygon-local p)
  (local (
          ; NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots-local p)
            (cond
              [(empty? (rest p)) MT]
              [else
               (render-line
                (connect-dots-local (rest p)) (first p) (second p))]))
          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line-local im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red")
            ))
    (render-line-local (connect-dots-local p) (first p) (last p))))

; WORDGAMES HEART =================================================================

(define w1 (list "h" "i"))
(define w2 (list "w" "h" "o"))

(check-expect
 (arrangements
  (list "w" "h"))
 (list 
  (list "w" "h")
  (list "h" "w")))

(check-expect
 (arrangements w1)
 (list
  (list "h" "i")
  (list "i" "h")))

(check-expect
 (arrangements '())
 (list '()))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else
     (local
       (
        ; 1String List-of-words -> List-of-words
        ; Add s to the start of every word in alow
        (define (append-all s alow)
          (cond
            [(empty? alow) '()]
            [else
             (cons
              (cons s (first alow))
              (append-all s (rest alow)))]
            ))
        ; 1String Word -> List-of-words
        ; insert s into every position of aw
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
        ; insert character into every position of all words in alow
        (define (insert-everywhere/in-all-words s alow)
          (cond
            [(empty? alow) '()]
            [else
             (append
              (insert-everywhere s (first alow))
              (insert-everywhere/in-all-words s (rest alow)))])))
       (insert-everywhere/in-all-words (first w)
                                       (arrangements (rest w))))]))

; =================================================================


; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (cond
         [(< (first l) smallest-in-rest) (first l)]
         [else smallest-in-rest]))]))

(define eg0 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define eg1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

(inf eg0)
(inf eg1)

(define-struct ir [price])
; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than $1

(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define first-inv (first an-inv))
             (define extracted-rest-inv (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons first-inv extracted-rest-inv)]
         [else extracted-rest-inv]))]))

; Lon -> Lon
; constructs a list from the items in l in descending order

(check-expect
 (sort> '()) '())

(check-expect
 (sort> '(2 3 4 6 2 34 ))
 '(34 6 4 3 2 2))

(define (sort> l0)
  (local  (; Lon -> Lon
           (define (sort l)
             (cond
               [(empty? l) '()]
               [else (insert (first l) (sort (rest l)))]))
           ; Number Lon -> Lon
           (define (insert an l)
             (cond
               [(empty? l ) (list an)]
               [else
                (cond
                  [(> an (first l)) (cons an l)]
                  [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))


; [List-of Number] -> [List-of Number]
; construct a lon from items in l in ascending order

(check-expect
 (sort-< '())
 '())

(check-expect
 (sort-< '(3 4 2  50 0 -3 9 8 ))
 '(-3 0 2 3 4 8 9 50))


(define (sort-< l0)
  (local
    (; Lon -> Lon
     ; sort
     (define (sort l)
       (cond
         [(empty? l) '()]
         [else
          (insert
           (first l)
           (sort (rest l)))]))
     ; Lon -> Lon
     ; insert n in ascending order
     (define (insert an l)
       (cond
         [(empty? l) (list an)]
         [else
          (if
           (< an (first l))
           (cons an l)
           (cons (first l) (insert an (rest l))))])))
    (sort l0)))

; sort-a

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; sort according to cmp

(check-expect
 (sort-a '(3 4 2  50 0 -3 9 8 ) <)
 (sort-< '(3 4 2  50 0 -3 9 8 )))

(check-expect
 (sort-a '(3 4 2  50 0 -3 9 8 ) >)
 (sort> '(3 4 2  50 0 -3 9 8 )))

(define (sort-a alon cmp)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(cmp an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort alon)))

(check-expect
 (sort-a '(3 4 2  50 0 -3 9 8 ) <)
 (sort-abs-< '(3 4 2  50 0 -3 9 8 )))

(check-expect
 (sort-a '(3 4 2  50 0 -3 9 8 ) >)
 (sort-abs> '(3 4 2  50 0 -3 9 8 )))

(define (sort-abs> l)
  (sort-a l >))

(define (sort-abs-< l)
  (sort-a l <))

(check-expect
 (sort-string-< '("hi" "atom" "zebra"))
 '("atom" "hi" "zebra"))

(check-expect
 (sort-string> '("hi" "atom" "zebra"))
 '("zebra" "hi" "atom"))

(define (sort-string-< alos)
  (sort-a alos string<?))

(define (sort-string> alos)
  (sort-a alos string>?))


; X [List-of X] -> [List-of X]
; put the x in the end of alox

(check-expect
 (insert-last '(0 0 1) '((1 0 0) (0 1 0)))
 '((1 0 0) (0 1 0) (0 0 1)))

(check-expect
 (insert-last 1 '(0 0 0))
 '( 0 0 0 1))

(check-expect
 (insert-last 0 '(1 0 0))
 '( 1 0 0 0))

(define (insert-last x alox)
  (cond
    [(empty? alox) (list x)]
    [else
     (cons
      (first alox)
      (insert-last x (rest alox)))]))

; MY-IDENTITY
; N -> [List-of [List-of Numbers]]
; make a identity matrix for a given matrix size

(check-expect
 (my-identity 0)
 '())

(check-expect
 (my-identity 1)
 '((1)))

(check-expect
 (my-identity 2)
 '((1 0) (0 1)))

(check-expect
 (my-identity 3)
 '((1 0 0) (0 1 0) (0 0 1)))

(check-expect
 (my-identity 4)
 '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))

(define (my-identity n)
  (cond [(zero? n) '()]
        [else
         (local (
                 ; LLon -> LLon
                 ; add a zero to every list in the list
                 (define (add-zero-to-end allon)
                   (cond
                     [(empty? allon) '()]
                     [else
                      (cons
                       (insert-last 0 (first allon))
                       (add-zero-to-end (rest allon)))]))

                 ; N -> Lon
                 ; create a list of zeros with n elements
                 ; they are all zeros except the last which is 1
                 (define (create-list n)
                   (cond
                     [(zero? n) '(1)]
                     [else
                      (cons
                       0
                       (create-list (sub1 n)))]))
                 )
           ; -- IN --
           (insert-last
            (create-list (sub1 n))
            (add-zero-to-end (my-identity (sub1 n)))))]))




























