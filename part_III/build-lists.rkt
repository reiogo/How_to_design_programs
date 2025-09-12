;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname build-lists) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Build Lists

; Number -> [List-of Number]
; create a list of nums from 0 to (n-1)

(check-expect
 (create-list 2)
 '(0 1))

(define (create-list n)
  (build-list n identity))

; Number -> [List-of Number]
; create a list of number from 1 to n

(check-expect
 (create-list-from-1 2)
 '(1 2))

(define (create-list-from-1 n)
  (build-list n add1))

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






















  