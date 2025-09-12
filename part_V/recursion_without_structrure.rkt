;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recursion_without_structrure) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Recursion without structure

; Answer to the four key questions for bundle

; - What is a trivially solvable problem
; -> a list of length n is the list
; -> an empty list is empty
; -> n equal zero is an error

; - How are trivial solutions solved?
; -> return the trivial case

; - How does the algorithm generate new problems
; that are more easy than the original, one or many?
; -> a function that isolates n from the list
; -> a function that excludes n from the list.

; - Is the solution to the given problem the same as the
; solution to the new problems?
; -> they need to be combined with implode and cons


; [List-of 1String] N -> [List-of String]
; bundles sub-sequences of s into strings of length n
; idea take and drop n items at a time
; FIXED -> termination (bundle s 0) loops unless s is '()

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3)
              (list "ab"))
(check-expect (bundle '() 3) '())
(check-error (bundle '("a") 0) "n cannot be zero")

(define (bundle s n)
  (cond
    [(zero? n) (error "n cannot be zero")]
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; retrieves the first n items in l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else
     (drop (rest l) (sub1 n))]))

; [List-of X] N -> [List-of [List-of X]]
; turns l into a list of chunks of size n

(check-expect
 (list->chunks (explode "123456789012") 6)
 (list (explode "123456") (explode "789012")))
(check-expect
 (list->chunks (explode "123") 2)
 '(("1" "2") ("3")))
(check-expect
 (list->chunks '() 2)
 '())

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (chunk l n)
      (list->chunks (drop-chunk l n) n))]))

; [List-of X] N -> [List-of X]
; chunk the first n elements and return it
(define (chunk l n)
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else
     (cons
      (first l)
      (chunk (rest l) (sub1 n)))]
    ))

; [List-of X] N -> [List-of X]
; remove the first n elements and return the rest
(define (drop-chunk l n)
  (cond
    [(empty? l) l]
    [(zero? n) l]
    [else
     (drop-chunk (rest l) (sub1 n))]))

(check-expect (my-bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (my-bundle '("a" "b") 3)
              (list "ab"))
(check-expect (my-bundle '() 3) '())

(define (my-bundle s n)
  (map (lambda (chunk) (implode chunk)) (list->chunks s n)))

; String N -> [List-of String] 
; partition s with chunks of size n

(check-expect (partition "abcd" 2) (list "ab" "cd"))
(check-expect (partition "abcd" 3) (list "abc" "d"))
(check-expect (partition "" 2) '())


(define (partition s n)
  (cond
    [(string=? s "") '()]
    [else
     (cons
      (take-partition s n)
      (partition (drop-partition s n) n))]))

; String N -> String
; make a partition from the first n
(define (take-partition s n)
  (cond
    [(zero? n) ""]
    [(string=? s "") ""]
    [else
     (string-append
      (substring s 0 1)
      (take-partition
       (substring s 1 (string-length s))
       (sub1 n)))]
    ))

; String N -> String
(define (drop-partition s n)
  (cond
    [(zero? n) s]
    [(string=? s "") s]
    [else
     (drop-partition
      (substring s 1 (string-length s))
      (sub1 n))]))



























