;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname two-lists) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Simultaneous processing

(define-struct employee [name ssn pay])
; An Employee is a structure.
; (make-employee String Number Number)
; interpretation, a struct with info about an employee
; pay is their hourly rate

(define-struct work [name hours])
; A Work is a structure.
; (make-work String Number)
; interpretation, a struct with how many hours a certain employee worked

; Worked is one of:
; - (list Employee Work)
(define w0 (list (make-employee "bob" 123 5.65) (make-work "bob" 40)))
(define w1 (list (make-employee "bobby" 123 8.75) (make-work "bob" 30)))


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
      (weekly-wage (first hours) (first hourly-wages))
      (wages*.v2 (rest hours) (rest hourly-wages)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours-worked
(define (weekly-wage pay-rate hours-worked)
  (* pay-rate hours-worked))

; [List-of Worked] -> [List-of [list String Number]]
; compute weekly wages for each person

(check-expect (wages*.v2.5 '()) '())
(check-expect (wages*.v2.5 (list w0)) '(("bob" 226.0)))
(check-expect (wages*.v2.5 (list w0 w1)) '(("bob" 226.0) ("bobby" 262.5)))

(define (wages*.v2.5 low)
  (cond
    [(empty? low) '()]
    [else
     (local ((define curr (first low))
             (define emp (first curr))
             (define wrk (second curr)))
       (cons
        (list
         (employee-name emp)
         (weekly-wage (employee-pay emp) (work-hours wrk)))
        (wages*.v2.5 (rest low))))]))

; 

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
; (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combines the list

(check-expect (zip '() '()) '())
(check-expect (zip '("bob") '("111 111 111"))
              (list (make-phone-record "bob" "111 111 111")))
(check-expect (zip '("bob" "dylan") '("111 111 111"
                                      "222 222 222"))
              (list
               (make-phone-record "bob" "111 111 111")
               (make-phone-record "dylan" "222 222 222")))

(define (zip los0 los1)
  (cond
    [(empty? los0) '()]
    [else
     (cons
      (make-phone-record (first los0) (first los1))
      (zip (rest los0) (rest los1)))]))

; N is one of:
; - 0
; - (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l;
; signals an error if there is no such symbol
(define ts  "list too short")

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) ts)
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) ts)
(check-error (list-pick (cons 'a '()) 3) ts)

(define (list-pick0 l n)
  (cond
    [(and (= n 0) (empty? l)) (error ts)]
    [(and (> n 0) (empty? l)) (error ts)]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l))
     (list-pick (rest l) (sub1 n))]))

(define (list-pick1 l n)
  (cond
    [(or (and (= n 0) (empty? l))
         (and (> n 0) (empty? l))) (error ts)]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l))
     (list-pick (rest l) (sub1 n))]))

(define (list-pick2 l n)
  (cond
    [(and (or (= n 0) (> n 0))
          (empty? l)) (error ts)]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l))
     (list-pick (rest l) (sub1 n))]))

(define (list-pick3 l n)
  (cond
    [(empty? l) (error ts)]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l))
     (list-pick (rest l) (sub1 n))]))

(define (list-pick l n)
  (cond
    [(empty? l) (error ts)]
    [(= n 0) (first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))

(define-struct branch [left right])
; A TOS (short for tree of symbols) is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A list of Directions is also called a path

; TOS Path -> Symbol
; traces the tree according to the path
; if encounters symbol before the path is finished: error

(check-expect
 (tree-pick (make-branch (make-branch 'a 'b) 'c)
            '(left left)) 'a)
(check-expect
 (tree-pick 'a '()) 'a)
(check-error
 (tree-pick 'a '(left)) "path incorrect")
(check-error
 (tree-pick 'a '(right)) "path incorrect")
(check-error
 (tree-pick (make-branch 'a 'b) '()) "path incorrect")
(check-expect
 (tree-pick (make-branch 'a 'b) '(left)) 'a)
(check-expect
 (tree-pick (make-branch 'a 'b) '(right)) 'b)

(define (tree-pick0 tos path)
  (cond
    [(and (symbol? tos) (empty? path)) tos]
    [(and
      (symbol? tos)
      (symbol=? 'left (first path))) (error "path incorrect")]
    [(and
      (symbol? tos)
      (symbol=? 'right (first path))) (error "path incorrect")]
    [(and (branch? tos) (empty? path)) (error "path incorrect")]
    [(and
      (branch? tos)
      (symbol=? 'left (first path)))
     (tree-pick (branch-left tos) (rest path))]
    [(and
      (branch? tos)
      (symbol=? 'right (first path)))
     (tree-pick (branch-right tos) (rest path))]))

(define (tree-pick tos path)
  (cond
    [(and (symbol? tos) (empty? path)) tos]
    [(or
      (and (symbol? tos) (symbol? (first path)))
      (and (branch? tos) (empty? path))) (error "path incorrect")]
    [(and
      (branch? tos)
      (symbol=? 'left (first path)))
     (tree-pick (branch-left tos) (rest path))]
    [(and
      (branch? tos)
      (symbol=? 'right (first path)))
     (tree-pick (branch-right tos) (rest path))]))


; [List-of Number] [List-of Number] -> [List-of Number]
; constucts a new list by replacing '() in front with end

(check-expect (replace-eol-with.v2 '() '(a b c)) '(a b c))
(check-expect (replace-eol-with.v2 '() '()) '())
(check-expect (replace-eol-with.v2 '(2 3 4) '()) '(2 3 4))
(check-expect (replace-eol-with.v2 (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with.v2 (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (replace-eol-with.v2 front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with.v2 (rest front) end))]))


; Two Complex Inputs ==================================================

; A LOD (list of directions) is one of:
; - '()
; - (cons Direction LOD)

(define-struct with [lft info rght])
(define-struct binary [lft rght])

; A TID (tree info directions) is one of:
; - Symbol
; - (make-binary TID TID)
; - (make-with TID Symbol TID)

















