;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recognizing_the_need_acc) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Recognizing the need for an Accumulator


; [List-of X] -> [List-of X]
; construct the reverse of alox

(check-expect (invert '(a b c)) '(c b a))

(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else (add-as-last (first alox) (invert (rest alox)))]))

; X [List-of X] -> [List-of X]
; add an-x to the end of alox

(check-expect (add-as-last 'a '(c b)) '(c b a))

(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons
      (first alox)
      (add-as-last an-x (rest alox)))]))

; [List-of X] -> [List-of X]
; construct the reverse of alox0

(check-expect (invert.v2 '(a b c)) '(c b a))

(define (invert.v2 alox0)
  (local (; [List-of X] [List-of X] -> [List-of X]
          ; construct the reverse of alox
          ; accumulator a is the list of all those items
          ; on alox0 that precede alox in reverse order
          (define (invert/a alox a)
            (cond
              [(empty? alox) a]
              [else (invert/a (rest alox) (cons (first alox) a))])))
    (invert/a alox0 '())))

; List-of-numbers -> List-of-numbers
; produces a sorted version of alon

(check-expect (sort> '(1 2 3)) '(3 2 1))
(check-expect (sort> '(-2 -2 1)) '(1 -2 -2))
(check-expect (sort> '(3 2 1)) '(3 2 1))
(check-expect (sort> '()) '())

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert (first alon) (sort> (rest alon)))]))

(time (invert.v2 (build-list 1000 identity)))
(time (invert (build-list 1000 identity)))


; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))






















