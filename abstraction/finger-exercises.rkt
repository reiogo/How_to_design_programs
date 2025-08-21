;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname finger-exercises) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Finger exercises

(define RATE 1.22) ; conversion rate, 1.22 euros per dollar

; [X -> Y] [List-of X] -> [List-of Y]
; [List-of Number] -> [List-of Number]
; convert a list of dollars to euros

(check-expect
 (convert-euro '( 1 3.4 3.94))
 (list (* RATE 1) (* RATE 3.4) (* RATE 3.94)))

(define (convert-euro lon)
  (local (; Number -> Number
          ; Convert value from dollar to euro
          (define (convert d)
            (* d RATE)))
    (map convert lon)))


; [List-of Fahrenheit] -> [List-of Celsius]
; Convert list of fahrenheit to list of celsius

(check-expect (convertFC '(212 50))
              '(100 10))

(define (convertFC lof)
  (local (; F -> C
          ; convert fahrenheit to celsius
          (define (convert f)
            (* (- f 32) (/ 5 9))))
    (map convert lof)))

; [List-of Posn] -> [List-of [list Number Number]]
; Translate a list of posns to a list of pairs of numbers

(check-expect
 (translate (list (make-posn 3 5) (make-posn 3 0)))
 '((3 5) (3 0)))

(define (translate lop)
  (local (; Posn -> [list Number Number]
          ; convert a posn to a pair of numbers in a list
          (define (convert p)
            (list (posn-x p) (posn-y p))))
    (map convert lop)))

(define-struct ir [name desc acqu-price sales-price])
; IR is a structure


; [List-of X] [X X -> Boolean] -> [List-of X]
; [List-of IR] -> [List-of IR]
; sort by the difference between the sales and acquisition price
; in descending order

(check-expect
 (sort-IR-diff
  (list
   (make-ir "hi" "hi" 10 4)
   (make-ir "hi" "hi" 13 5)
   (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "hi" "hi" 10 1)
  (make-ir "hi" "hi" 13 5)
  (make-ir "hi" "hi" 10 4)
  ))


(define (sort-IR-diff loir)
  (local (; IR IR -> Boolean
          ; is ir0 bigger than ir1
          (define
            (greater? ir0 ir1)
            (> (- (ir-acqu-price ir0) (ir-sales-price ir0))
               (- (ir-acqu-price ir1) (ir-sales-price ir1)))
            ))
    (sort loir greater? )))

; [X -> Boolean] [List-of X] -> [List-of X]
; Number [List-of IR] -> [List-of IR]
; eliminate IR from list with sales price below ua

(check-expect
 (eliminate-exp 4
                (list
                 (make-ir "hi" "hi" 10 4)
                 (make-ir "hi" "hi" 13 5)
                 (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "hi" "hi" 10 4)
  (make-ir "hi" "hi" 13 5)))

(define (eliminate-exp ua loir)
  (local (; IR -> Boolean
          ; check if sales price is greater than ua
          (define (greater-ua? ir0)
            (>= (ir-sales-price ir0) ua)))
    (filter greater-ua? loir)))

; String [List-of IR] -> [List-of IR]
; make a list of ir that doesn't include items named ty

(check-expect
 (recall "hi"
         (list
          (make-ir "hi" "ie" 10 4)
          (make-ir "hi" "hi" 13 5)
          (make-ir "hi" "hi" 10 1)))
 '())
(check-expect
 (recall "hi"
         (list
          (make-ir "hi" "ie" 10 4)
          (make-ir "i" "hi" 13 5)
          (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "i" "hi" 13 5)))

(define (recall ty loir)
  (local (; IR -> Boolean
          ; check that ir0 doesn't have ty as item name
          (define (not-ty? ir0)
            (not (string=? (ir-name ir0) ty))))
    (filter not-ty? loir)))

; [List-of String] [List-of String] -> [List-of String]
; make a list that is a union of the two given

(check-expect
 (selection '("hi" "i" "h") '("hi"))
 '("hi"))

(define (selection los0 los1)
  (local(; String [List-of String] -> Boolean
         ; Check if s1 is on los0
         (define (on-first-list? s1)
           (member? s1 los0)))
    (filter on-first-list? los1)
    ))

; [X -> Boolean] [List-of X] -> Boolean
; ormap

; String [List-of String] -> Boolean
; checks whether name is an element on lonames or an extension of it.

(check-expect
 (find-name "hi"
            '( "hi" "Hello"))
 #true)

(check-expect
 (find-name "hi"
            '( "hii" "Hello"))
 #true)

(check-expect
 (find-name "hi"
            '("Hello"))
 #false)

; String String -> Boolean
; check if s1 is an extension of s0

(check-expect
 (extension? "hi" "hii")
 #true)

(check-expect
 (extension? "hi" "hi")
 #true)

(check-expect
 (extension? "hi" "h")
 #false)


(define (extension? s0 s1)
  (cond [(string=? "" s0) #true]
        [(string=? "" s1) #false]
        [else
         (and
          (string=? (substring s0 0 1)(substring s1 0 1))
          (extension?
           (substring s0 1 (string-length s0))
           (substring s1 1 (string-length s1))))]))


(define (find-name name lonames)
  (local (; String -> Boolean
          ; true if s is an extension of name
          (define (valid-name? s)
            (cond
              [(extension? name s) #true]
              [else #false]))
          )
    (ormap valid-name? lonames)))

; [X -> Boolean][List-of X] -> Boolean
; andmap

; [List-of String] -> Boolean
; Check that all the names start with a

(check-expect
 (all-a '("addis" "addidas"))
 #true)

(check-expect
 (all-a '("daddis" "addidas"))
 #f)

(define (all-a los)
  (local (; String -> Boolean
          ; Check if string starts with "a"
          (define (one-a s)
            (string=?
             "a"
             (substring s 0 1)))
          )
    (andmap one-a los)))


; [X Y -> Y] Y [List-of X] -> Y

; APPEND-FROM-FOLD
; [List-of X] [List-of Y] -> [List-of XandY]
; define append using foldr

(check-expect
 (append-from-fold '(1 2 3) '(4 5 6 7))
 '(1 2 3 4 5 6 7))

(check-expect
 (append-from-fold '(1 2 ) '(4 5 6 7))
 '(1 2 4 5 6 7))

(define (append-from-fold lox loy)
  (foldr cons loy lox))

; [List-of Number] -> Number
; compute sum

(check-expect
 (fold-sum '(2 3 45))
 50)

(define (fold-sum lon)
  (foldr + 0 lon))

; [List-of Number] -> Number
; compute prod

(check-expect
 (fold-prod '( 3 5 6))
 90)

(define (fold-prod lon)
  (foldr * 1 lon))

;(check-expect (hori
;               (list (circle 5 "solid" "blue")
;                     (circle 5 "solid" "green")
;                     (circle 5 "solid" "red")))
;              #true)

(define (hori loi)
  (foldl above empty-image loi))

; [X -> Y] [List-of X] -> [List-of Y]
; define map using fold

(check-expect
 (fold-map add1 '(1 2 3))
 (map add1 '(1 2 3)))

(define (fold-map func lox)
  (local (; X Y -> Y
          ; use func on x and cons the result with y
          (define (apply-func x y)
            (cons (func x) y)))
    (foldr apply-func '() lox)))

; [List-of 1String] -> [List-of [List-of 1String]]

(check-expect
 (prefixes
  (list "a" "b" "c"))
 (list
  (list "a")
  (list "a" "b")
  (list "a" "b" "c")))

(check-expect
 (prefixes
  (list "b" "c"))
 (list
  (list "b")
  (list "b" "c")))

(check-expect
 (prefixes
  (list  "c"))
 (list
  (list "c")))


(define (prefixes alos)
  (cond
    [(empty? alos) '()]
    [else
     (cons 
      (list (first alos))
      (local (; [List-of 1String] -> [List-of 1String]
              ; insert (first alos) into beginning given element
              (define (insert-start word)
                (cons (first alos) word)))
        (map insert-start (prefixes (rest alos)))))]))

; [List-of 1String] -> [List-of [List-of 1String]]

(check-expect
 (suffixes
  (list "a" "b" "c" "d"))
 (reverse (list
  (list "d")
  (list "c" "d" )
  (list "b" "c" "d")
  (list "a" "b" "c" "d"))))

(check-expect
 (suffixes
  (list "a" "b" "c"))
 (reverse (list
  (list "c")
  (list "b" "c")
  (list "a" "b" "c")
  )))

(check-expect
(suffixes
  (list "a" "b"))
 (reverse (list
  (list "b")
  (list "a" "b"))))

(check-expect
  (suffixes
  (list  "a"))
 (reverse (list
  (list "a"))))


(define (suffixes alo1s)
  (local (
          ; [List-of 1String -> [List-of [List-of 1String]]
          ; ...
          (define (suffixes a0)
            (cond
              [(empty? a0) '()]
              [else
               (cons
                a0
                (suffixes (rest a0)))]))
          )
     (suffixes alo1s)))


;; [List-of X] -> [List-of [List-of X]]
;; creates a list of all suffixes of alo1s
;(define (suffixes alo1s)
;  (local
;    (; [List-of X] -> [List-of [List-of X]]
;     ; creates a list of all suffixes of alo1s in descending order of length
;     (define (suffixes-helper sublist)
;       (cond
;         [(empty? sublist) '()]
;         [else
;          (cons
;           sublist
;           (suffixes-helper (rest sublist)))]))
;
;    ; [List-of [List-of X]] -> [List-of [List-of X]]
;    ; this helper function is for folding over the list of suffixes
;    (define (my-cons x acc)
;      (cons x acc)))
;
;    ; -- IN --
;    (foldr my-cons '() (suffixes-helper alo1s))))












