;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname parsing) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; parsing

; A File is one of:
; - '()
; - (cons "\n" File)
; - (cons 1String File)
; interpretation represents a file with
; "\n" as the newline character


; A Line is a [List-of 1String].

; (list "h" "i" "\n") -> (list (list "h" "i"))
; '() -> '()
; (list "\n") -> (list '() '())
; (list "\n" "\n")
;      -> (list '() '() '())

(define NEWLINE "\n")

; File -> Line
; retrieves the prefix of afile up to the first occurence of NEWLINE
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))

; File -> Line
; drops the suffix of afile behind the first occurence of NEWLINE
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))

         
; File -> [List-of Line]
; converts a file into a list of lines

(check-expect (file->list-of-lines
               (list "a" "b" "c" "\n" "d" "e" "\n" "f"
                     "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
; 4 generative questions:
; if file is '()
; file doesn't contain a line
; else the file contains at least one "\n" or one 1String
; these will be separated into one line, the rest is a new problem
; cons the initial segment as a single line to the rest



; A Token is a either a 1String or a String that consists of
; lower-case letters and nothing else

; Line -> String
; Bundle the first word or 1String

(check-expect (bundle-first '("a" "b" " "))
              "ab")

(define (bundle-first l)
  (cond
    [(empty? l) ""]
    [(string-lower-case? (first l))
     (string-append (first l) (bundle-first (rest l)))]
    [else ""]))

; Line -> Line
; remove the first word/1String

(check-expect (remove-first '("a" "b" " "))
              '(" "))

(define (remove-first l)
  (cond
    [(empty? l) '()]
    [(string-lower-case? (first l))
     (remove-first (rest l))]
    [else l]))


; Line -> [List-of Token]
; generative: the line will be split into
; words, 1Strings, and whitespace (or empty). Then these elements
; will be consed together
; termination: Each recursive call is shrinking the
; line by at least one character.

(check-expect (tokenize '("a" "b" " " "."))
              '("ab" "."))
(check-expect (tokenize '("a" "b" " " "." "h" "e"))
              '("ab" "." "he"))
(check-expect (tokenize '("a" "b" " " "." "h" "e"))
              '("ab" "." "he"))

(define (tokenize l)
  (cond
    [(empty? l) '()]
    [(string-lower-case? (first l))
     (cons (bundle-first l)
           (tokenize (remove-first l)))]
    [(string-whitespace? (first l)) (tokenize (rest l))]
    [else (cons (first l)
                (tokenize (rest l)))]))

; 4 generative questions:
; if a line is a one
; then the list of tokens is empty
; else the file contains at least one 1String
; consecutive letters will be bundled up into words and
; non-letters will be as is or if they are whitespace, removed
; They will all be consed together

; Number [List-of Number] -> [List-of Number]
; make a row
(define (make-row n0 lon0)
  (cond
    [(zero? n0) '()]
    [else
     (cons (first lon0)
           (make-row (sub1 n0) (rest lon0)))]))

; Number [List-of Number] -> [List-of Number]
; remove first n numbers from lon
(define (remove-row n1 lon1)
  (cond
    [(zero? n1) lon1]
    [else
     (remove-row (sub1 n1) (rest lon1))]))

; Number [List-of Number] -> [List-of [List-of Number]]
; makes a matrix
; generative: create row from the first n numbers
; pass the rest recursively
; termination: lon recursively shrinks

(check-expect
 (create-matrix 2 '(1 2 3 4))
 '((1 2)
   (3 4)))
(check-expect
 (create-matrix 0 '())
 '())
(check-expect
 (create-matrix 3 '(1 2 3 4 5 6 7 8 9))
 '((1 2 3)
   (4 5 6)
   (7 8 9)))

(define (create-matrix n lon)
  (cond
    [(empty? lon) '()]
    [else
     (cons
      (make-row n lon)
      (create-matrix n (remove-row n lon)))]))

; trivial case is a 0 matrix
; return empty list
; generate new problems by
; taking the first n numbers and creating a list with it
; the end result comes from consing the lists together


















