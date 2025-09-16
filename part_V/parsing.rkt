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
(define (bundle-first l)
  (cond
    [(not (string-lower-case? (first l))) ""]
    [else
     (string-append (first l) (bundle-first (rest l)))]))

; Line -> Line
; remove the first word/1String
(define (remove-first l)
  (cond
    [(empty? l) '()]
    [(not (string-lower-case? (first l))) l]))


; Line -> [List-of Token]
(define (tokenize l)
  '())

; 4 generative questions:
; if a line is empty
; then the list of tokens is empty
; else the file contains at least one 1String
; consecutive letters will be bundled up into words and
; non-letters will be as is or if they are whitespace, then removed
; They will all be consed together


















