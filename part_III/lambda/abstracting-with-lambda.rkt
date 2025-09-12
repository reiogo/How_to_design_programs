;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstracting-with-lambda) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Abstracting with Lambda

(define DOT (circle 2 "solid" "red"))
(define BG (empty-scene 100 100))

(define (dots lop)
  (local (;Posn Image -> Image
          (define (add-one-dot p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    (foldr add-one-dot BG lop)))

(define (dots-lambda lop)
  (foldr (lambda (one-posn scene)
           (place-image DOT (posn-x one-posn) (posn-y one-posn) scene))
         BG lop))

; [List-of Posn] -> [List-of Posn]
; (add-3-to-all (list (make-posn 30 10) (make-posn 0 0)))
; should deliver
; (list (make-posn 33 10) (make-posn 3 0))

(define (add-3-to-all lop)
  (map (lambda (p) (make-posn (+ (posn-x p) 3) (posn-y p))) lop))

; [List-of Posn] -> [List-of Posn]
; (keep-good (list (make-posn 0 110) (make-posn 0 60)))
; should deliver
; (list (make-posn 0 60))

(define (keep-good lop)
  (filter (lambda (p) (<= (posn-y p) 100)) lop))

; [List-of Posn] -> Boolean
; (close? (list (make-posn 3 4) (make-posn 9 9))(make-posn 0 0))
; should be
; #true

;(define (close? lop pt)
;  (ormap (lambda (p) (close-to p pt CLOSENESS)) lop))
;(define CLOSENESS 5) ; in terms of pixels


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

(check-expect
 (convert-euro-l '( 1 3.4 3.94))
 (list (* RATE 1) (* RATE 3.4) (* RATE 3.94)))

(define (convert-euro-l lon)
  (map (lambda (d) (* d RATE)) lon))

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

(check-expect (convertFC-l '(212 50))
              '(100 10))
(define (convertFC-l lof)
  (map (lambda (f) (* (- f 32) (/ 5 9))) lof))


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

(check-expect
 (translate-l (list (make-posn 3 5) (make-posn 3 0)))
 '((3 5) (3 0)))

(define (translate-l lop)
  (map (lambda (p) (list (posn-x p) (posn-y p))) lop))

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


(check-expect
 (sort-IR-diff-l
  (list
   (make-ir "hi" "hi" 10 4)
   (make-ir "hi" "hi" 13 5)
   (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "hi" "hi" 10 1)
  (make-ir "hi" "hi" 13 5)
  (make-ir "hi" "hi" 10 4)
  ))

(define (sort-IR-diff-l loir)
  (sort loir
        (lambda (ir0 ir1)
          (> (- (ir-acqu-price ir0)
                (ir-sales-price ir0))
             (- (ir-acqu-price ir1)
                (ir-sales-price ir1))))))


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

(check-expect
 (eliminate-exp-l 4
                  (list
                   (make-ir "hi" "hi" 10 4)
                   (make-ir "hi" "hi" 13 5)
                   (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "hi" "hi" 10 4)
  (make-ir "hi" "hi" 13 5)))

(check-expect
 (eliminate-exp-l 0
                  (list
                   (make-ir "hi" "hi" 10 4)
                   (make-ir "hi" "hi" 13 5)
                   (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "hi" "hi" 10 4)
  (make-ir "hi" "hi" 13 5)
  (make-ir "hi" "hi" 10 1)))


(define (eliminate-exp-l ua loir)
  (filter (lambda (ir0) (>= (ir-sales-price ir0) ua)) loir))

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

(check-expect
 (recall-l "hi"
           (list
            (make-ir "hi" "ie" 10 4)
            (make-ir "hi" "hi" 13 5)
            (make-ir "hi" "hi" 10 1)))
 '())
(check-expect
 (recall-l "hi"
           (list
            (make-ir "hi" "ie" 10 4)
            (make-ir "i" "hi" 13 5)
            (make-ir "hi" "hi" 10 1)))
 (list
  (make-ir "i" "hi" 13 5)))


(define (recall-l ty loir)
  (filter (lambda (ir0) (not (string=? (ir-name ir0) ty))) loir))

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

(check-expect
 (selection-l '("hi" "i" "h") '("hi"))
 '("hi"))

(define (selection-l los0 los1)
  (filter (lambda (s1) (member? s1 los0)) los1))


; Build Lists

; Number -> [List-of Number]
; create a list of nums from 0 to (n-1)

(check-expect
 (create-list 2)
 '(0 1))

(define (create-list n)
  (build-list n identity))

(check-expect
 (create-list-l 2)
 '(0 1))

(define (create-list-l n)
  (build-list n (lambda (x) x)))

; Number -> [List-of Number]
; create a list of number from 1 to n

(check-expect
 (create-list-from-1 2)
 '(1 2))

(define (create-list-from-1 n)
  (build-list n add1))

(check-expect
 (create-list-from-1-l 2)
 '(1 2))

(define (create-list-from-1-l n)
  (build-list n (lambda (x) (+ x 1))))

; Number -> [List-of Number]
; create a list of numbers where each number is divide by 10 of the prev

(check-expect
 (div10-list 3)
 '( 1 1/10 1/100))

(define (div10-list n)
  (local (; Number -> Number
          ; Div by 10
          (define (div10 n0)
            (/ 1 (expt 10 n0))))
    (build-list n div10)))

(check-expect
 (div10-list-l 3)
 '( 1 1/10 1/100))

(define (div10-list-l n)
  (build-list n (lambda (n0) (/ 1 (expt 10 n0)))))

; Number -> [List-of Number]
; creates list of first n even numbers

(check-expect
 (even-list 3)
 '(0 2 4))
(check-expect
 (even-list 6)
 '(0 2 4 6 8 10))

(define (even-list n)
  (local (; Number -> Number
          ; make even
          (define (even-ify n0)
            (* n0 2))
          )
    (build-list n even-ify)))

(check-expect
 (even-list-l 3)
 '(0 2 4))
(check-expect
 (even-list-l 6)
 '(0 2 4 6 8 10))

(define (even-list-l n)
  (build-list n (lambda (n0) (* n0 2))))

; Number -> [List-of [List-of 1or0]]
; make an identity matrix where the row/column value is n

(check-expect
 (iden-matrix 1)
 '((1)))

(check-expect
 (iden-matrix 2)
 '((1 0) (0 1)))

(check-expect
 (iden-matrix 3)
 '((1 0 0) (0 1 0) (0 0 1)))


(define (iden-matrix n)
  (local (
          ; [List-of 0] N -> [List-of 1or0]
          ; toggle nth (0-index) place as 1
          (define (toggle-zero nth loz)
            (cond
              [(empty? loz) '()]
              [else
               (if
                (= nth 0)
                (cons 1 (toggle-zero (sub1 nth) (rest loz)))
                (cons 0
                      (toggle-zero (sub1 nth) (rest loz))))]))
          ; Number -> [List-of 1or0]
          ; create a list of 1 and 0s where 1 is at the nth place
          (define (make-zero-list n0)
            (toggle-zero n0 (build-list n identity)))
          )
    (build-list n make-zero-list)))

(check-expect
 (iden-matrix-l 1)
 '((1)))

(check-expect
 (iden-matrix-l 2)
 '((1 0) (0 1)))

(check-expect
 (iden-matrix-l 3)
 '((1 0 0) (0 1 0) (0 0 1)))

(define (iden-matrix-l n)
  (build-list
   n
   (lambda (n0)
     (build-list
      n
      (lambda (n1) (if (= n1 n0) 1 0))))))

; Number -> [List-of Number]
; tabulate f between n and 0 in a list

(check-expect
 (tabulate add1 3)
 (list
  1
  2
  3))

(define (tabulate f n)
  (build-list n f))

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

(check-expect
 (find-name-l "hi"
            '( "hi" "Hello"))
 #true)

(check-expect
 (find-name-l "hi"
            '( "hii" "Hello"))
 #true)

(check-expect
 (find-name-l "hi"
            '("Hello"))
 #false)


(define (find-name-l name lonames)
  (ormap (lambda (s) (if (extension? name s) #true #false)) lonames))



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

(check-expect
 (all-a-l '("addis" "addidas"))
 #true)

(check-expect
 (all-a-l '("daddis" "addidas"))
 #f)

(define (all-a-l los)
  (andmap (lambda (s) (string=? "a" (substring s 0 1))) los))

(check-expect
 (append-from-fold '(1 2 3) '(4 5 6 7))
 '(1 2 3 4 5 6 7))

(check-expect
 (append-from-fold '(1 2 ) '(4 5 6 7))
 '(1 2 4 5 6 7))

(define (append-from-fold lox loy)
  (foldr cons loy lox))



















  























