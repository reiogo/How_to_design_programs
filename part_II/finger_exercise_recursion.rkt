;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname finger_exercise_recursion) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)
; interpretation a List-of-amounts represents a sequence of amounts
; Examples:
; '()
; (cons 5 '())
; (cons 5 (cons 4 '()))

; List-of-amounts -> Number
; Sum the numbers in the list of amounts
; given empty expect 0
(check-expect (sum '()) 0)
; given non-empty expect sum
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 1 (cons 4 '()))) 5)
(define (sum aloa)
  (cond
    [(empty? aloa) 0]
    [else (+ (first aloa) (sum (rest aloa)))]))
; (sum (cons 1 (cons 2 '())))

; A List-of-numbers is one of :
; - '()
; - (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; Check whether every number in the lon is positive
; given '() expect true
(check-expect (pos? '()) #true)
; given (cons -1 (cons 33 '())) expect false
(check-expect (pos? (cons -1 (cons 33 '()))) #false)
(check-expect (pos? (cons -1 '())) #false)
; given (cons 1 (cons 3 '())) expect true
(check-expect (pos?(cons 1 (cons 3 '()))) #true)
(check-expect (pos?(cons 1 '())) #true)
; given (cons 0 (cons 3 '())) expect false
(check-expect (pos? (cons 0 (cons 3 '()))) #false)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else
     (and (> (first lon) 0) (pos? (rest lon)))]))


; (pos? (cons 5 '()))
; (pos? (cons -1 '()))

; List-of-numbers -> Number
; Checks if the given numbers are all positive before summing them
(check-error
 (checked-sum (cons -1 '()))
 "expected aloa but got alon instead")
(check-expect
 (checked-sum (cons 1 '())) 1)

(define (checked-sum alon)
  (if (pos? alon)
      (sum alon)
      (error "expected aloa but got alon instead")))


; A List-of-booleasn is one of:
; '()
; (cons Boolean List-of-booleans)
; interpretation, a list-of-booleans
; represents a list of true or false values

; List-of-booleans -> Boolean
; Determines whether all of the booleans in the list are true or not
; given '() expect true
(check-expect (all-true '()) #true)
; given (cons #false '()) expect false
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
; given (cons #true '()) expect true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)

(define (all-true alob)
  (cond [(empty? alob) #true]
        [else
         (and (first alob) (all-true (rest alob)))]))

; List-of-booleans -> Boolean
; Determines if even one of the booleans are true
(check-expect (one-true '()) #false)
; given (cons #false '()) expect false
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #false (cons #false '()))) #false)
; given (cons #true '()) expect true
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)

(define (one-true alob)
  (cond [(empty? alob) #false]
        [else
         (or (first alob) (one-true (rest alob)))]))

; List-of-string -> String
; concatenate all strings in 1 into one long string
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect (cat (cons "ab" (cons "cd" (cons "ef" '())))) "abcdef")
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))
; (cat (cons "a" '()))

; A List-of-images is one of:
; - '()
; - (cons Image list-of-images)
; interpretation a list-of-images is a sequence of images.

; An ImageOrFalse is one of:
; - #false
; - Image
; interpretation, a definition for the lack of a adaquate image


; list-of-images Number -> ImageOrFalse
; find the first image that
; is not an n by n square in proportion
(check-expect (ill-sized? '() 100) #false)
(check-expect
 (ill-sized? (cons (empty-scene 100 100) '()) 100)
 #false)
(check-expect
 (ill-sized?
  (cons (empty-scene 100 100)
        (cons (empty-scene 100 100) '())) 100)
 #false)
(check-expect
 (ill-sized? (cons (empty-scene 99 9)
                   (cons (empty-scene 100 100) '()))
             100)
 (empty-scene 99 9))
(check-expect
 (ill-sized? (cons (empty-scene 99 100) '()) 100)
 (empty-scene 99 100))


(define (ill-sized? aloi n)
  (cond
    [(empty? aloi) #false]
    [else
     (cond
       [(img-not-nxn (first aloi) n) (first aloi)]
       [else (ill-sized? (rest aloi) n)])]))

; Image Number -> Boolean
; check if image is not n by n square in proportion.
(check-expect
 (img-not-nxn (empty-scene 100 100) 100) #false)
(check-expect
 (img-not-nxn (empty-scene 100 100) 99) #true)
(define (img-not-nxn img n)
  (not (and
        (= (image-width img) n)
        (= (image-height img) n))))

















