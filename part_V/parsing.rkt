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
; '() -> (list (list ""))
; (list "\n") -> (list (list "") (list ""))
; (list "\n" "\n")
;      -> (list (list "") (list "") (list ""))

; File -> [List-of Line]
; converts a file into a list of lines

(check-expect (file->list-of-lines
               (list "a" "b" "c" "\n" "d" "e" "\n" "f"
                     "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  '())


















