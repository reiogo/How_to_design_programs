;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Fixing:optimizing_dicts) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct letter-counts [letter n])


; LLoS -> letter-counts
; Find the list with the most entries

(check-expect
 (max-dict
  (list
   (list "apple" "adonis")
   (list "be")))
 (list "apple" "adonis"))

(check-expect
 (max-dict
  (list '()))
 '())

(define (max-dict allos)
  (cond
    [(empty? (rest allos)) '()]
    [else
     (if
      (greatest? (first allos) (rest allos))
      (first allos)
      (max-dict (rest allos)))]))

; Los LLoS -> boolean
; Checks if alos is the greatest in allos

(check-expect
 (greatest? (list "apple" "adonis")
            (list
             (list "be")
             (list "c")))
 #true)

(check-expect
 (greatest? (list "apple" "adonis")
            (list
             (list "be")
             (list "c" "cat" "chamber")))
 #false)

(check-expect
 (greatest? (list "apple" "adonis")
            (list
             (list "be")
             (list "cat")))
 #true)

(check-expect
 (greatest? (list "apple" "adonis")
            '())
 #true)

(define (greatest? alos allos)
  (cond
    [(empty? allos) #true]
    [else
     (and
      (> (length alos) (length (first allos)))
      (greatest? alos (rest allos)))]))


; List-of-letter-counts -> letter-counts
; Finds the letter-count with the largest frequency

(check-expect
 (max-frequent
  (list
   (make-letter-counts "a" 0)
   (make-letter-counts "b" 2)
   (make-letter-counts "c" 3)))
 (make-letter-counts "c" 3))

(check-expect
 (max-frequent
  '())
 (make-letter-counts "" 0))
 
(define (max-frequent lolc)
  (cond
    [(empty? lolc) (make-letter-counts "" 0)]
    [else
     (if
      (greatest-letter-counts (first lolc) (rest lolc))
      (first lolc)
      (max-frequent (rest lolc)))]))

; letter-counts list-of-letter-counts -> boolean
; check if alc is the greatest in alolc

(check-expect
 (greatest-letter-counts
  (make-letter-counts "a" 1)
  (list
   (make-letter-counts "b" 3)
   (make-letter-counts "c" 5)
   (make-letter-counts "d" 0)))
 #false)

(check-expect
 (greatest-letter-counts
  (make-letter-counts "a" 6)
  (list
   (make-letter-counts "b" 3)
   (make-letter-counts "c" 5)
   (make-letter-counts "d" 0)))
 #true)

(check-expect
 (greatest-letter-counts
  (make-letter-counts "a" 6)
  '())
 #true)

(define (greatest-letter-counts alc alolc)
  (cond
    [(empty? alolc) #true]
    [else
     (and
      (> (letter-counts-n alc) (letter-counts-n (first alolc)))
      (greatest-letter-counts alc (rest alolc)))]))












