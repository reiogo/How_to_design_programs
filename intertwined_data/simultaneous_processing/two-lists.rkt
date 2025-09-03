;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname two-lists) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Simultaneous processing
; [List-of Number] [List-of Number] -> [List-of Number]
; constucts a new list by replacing '() in front with end

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons
      (first front)
      (replace-eol-with (rest front) end))]))

; [List-of Symbol] [List-of Number] -> [List-of [List Symbol Number]]
; crosses ever symbol with every number

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else
     (append
      (map (lambda (n) (list (first los) n)) lon)
      (cross (rest los) lon))]))

; [List-of Number] [List-of Number] -> [List-of Number]
; computes weekly wages by multiplying the corresponding
; items on hours and hourly-wages
; assume the two lists are of equal length

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 '(5.65) '(40)) '(226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0)) '(226.0 262.5))

(define (wages*.v2 hours hourly-wages)
  (cond
     [(empty? hours) '()]
     [else
      (cons
       (* (first hours) (first hourly-wages))
       (wages*.v2 (rest hours) (rest hourly-wages)))]))




















