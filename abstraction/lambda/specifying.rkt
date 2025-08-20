;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname specifying) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Specifying with lambda

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; produces a function that determines whether
; some list is sorted according to cmp

(check-expect
 [(sorted string<?) '()] #true)

(check-expect
 [(sorted string<?) '("a" "b" "c")] #true)

(check-expect
 [(sorted <) '(1 2 3 4 5 6 7 8)] #true)

(check-expect
 [(sorted <) '(1 2 3 4 5 6 8 7)] #false)

(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is the given list l0 sorted according to cmp
            (define (sorted/l l)
              (cond [(empty? (rest l)) #true]
                    [else
                     (and
                      (cmp (first l) (second l))
                      (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

; [X X -> Boolean] [NEList-of X] -> Boolean
; determine whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (sorted? cmp l)
  (cond [(empty? (rest l)) #true]
        [else
         (and
          (cmp (first l) (second l))
          (sorted? cmp (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort-cmp/bad l)
  '(9 8 7 6 5 4 3 2 1 0))

; [List-of X] [X X -> Boolean] -> [ [List-of X ] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0

(check-expect
 [(sorted-variant-of '(1 3 2) <) '(1 2 3)] #t)

(check-expect
 [(sorted-variant-of '(1 3 2) <) '(1 3)] #f)

(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains l0 k))))
; [List-of X] [List-of X] -> Boolean
; are all items in list k members of list l

(check-expect (contains? '(1 2 3) '(2 1 4 3)) #f)
(check-expect (contains? '(1 2 3 4) '(2 1 3)) #t)

(define (contains? l k)
  (andmap (lambda (item-in-k) (member? item-in-k l)) k))
























