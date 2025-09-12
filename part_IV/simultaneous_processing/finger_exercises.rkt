;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname finger_exercises) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; finger exercises

; A Son is one of :
; - empty
; - (cons Number Son)

; Son Son -> Boolean
; check if the sets are the same

(check-expect (sameset? '(1 2 3) '(1 2 3)) #t)
(check-expect (sameset? '(1) '(1 2 3)) #f)
(check-expect (sameset? '(1 2 3) '(3)) #f)

(define (sameset? l0 l1)
  (local (; N Son -> Son
          ; remove x from l
          (define (remove x l)
            (filter (lambda (n) (if (= n x) #f #t)) l))
          )
    (cond
      [(empty? l0) (if (empty? l1) #true #false)]
      [else
       (if (member? (first l0) l1)
           (sameset? (rest l0) (remove (first l0) l1))
           #false)])))

(define (testset l1)
  (lambda (l0) (sameset? l0 l1)))

; Son Son -> Son
; union of two sets

(check-satisfied (union '( 1 2 4) '(1 2 5))
                 (testset '(1 2 4 5)))
(check-satisfied (union '( 3 1 2 4) '(1 6 2 5))
                 (testset '( 3 1 6 2 4 5)))
(check-satisfied (union '() '(1 2 5))
                 (testset '(1 2 5)))
(check-satisfied (union '( 1 2 4) '())
                 (testset '(1 2 4)))


(define (union lon0 lon1)
  (cond
    [(empty? lon0) lon1]
    [else
     (if
      (member? (first lon0) lon1)
      (union (rest lon0) lon1)
      (cons (first lon0) (union (rest lon0) lon1)))]))

; Son Son -> Son
; intersect of two sets

(check-satisfied (intersect '( 1 2 4) '(1 2 5))
                 (testset '(1 2)))
(check-satisfied (intersect '( 3 1 2 4) '(1 6 2 5))
                 (testset '(1 2)))
(check-satisfied (intersect '() '(1 2 5))
                 (testset '()))
(check-satisfied (intersect '( 1 2 4) '())
                 (testset '()))

(define (intersect lon0 lon1)
  (cond
    [(empty? lon0) '()]
    [else
     (if
      (member? (first lon0) lon1)
      (cons (first lon0) (intersect (rest lon0) lon1))
      (intersect (rest lon0) lon1))
     ]))

; [List-of Numbers] [List-of Numbers] -> [List-of Numbers]
; takes two sorted lons (asc) and merges them in the correct order

(check-expect (merge '(1 2 3 6) '(5 9))
              '(1 2 3 5 6 9))
(check-expect (merge '() '(5 9))
              '(5 9))
(check-expect (merge '(1 2 3 6) '())
              '(1 2 3 6))
(check-expect (merge '() '())
              '())


(define (merge lon0 lon1)
  (cond
    [(empty? lon0) lon1]
    [(empty? lon1) lon0]
    [else
     (if (< (first lon0) (first lon1))
         (cons (first lon0) (merge (rest lon0) lon1))
         (cons (first lon1) (merge lon0 (rest lon1))))]))

; [List-of Any] N -> [List-of Any]
; remove the first n items

(check-expect (drop '() 0) '())
(check-expect (drop '() 2) '())
(check-expect (drop '("h" "h" "i") 2) '("i"))
(check-expect (drop '("h" "h" "i") 4) '())
(check-expect (drop '("h" "h" "i") 0) '("h" "h" "i"))

(define (drop l n)
  (cond
    [(or (empty? l) (= n 0)) l]
    [else
     (drop (rest l) (sub1 n))]))

; [List-of Any] N -> [List-of Any]
; gets the first n numbers or the whole list if n is
; longer than the list

(check-expect (take '() 0) '())
(check-expect (take '() 2) '())
(check-expect (take '("h" "h" "i") 2) '("h" "h"))
(check-expect (take '("h" "h" "i") 4) '("h" "h" "i"))
(check-expect (take '("h" "h" "i") 0) '())

(define (take l n)
  (cond
    [(or (empty? l) (= n 0)) '()]
    [else
     (cons
      (first l)
      (take (rest l) (sub1 n)))]))



(define-struct employee [name eid pay-rate])
; An Employee is a structure
; (make-employee String Number Number)
; interpretation (make-employee n e p)
; n is the employees name, eid is their id
; p is pay per hour worked

(define-struct punchcard [eid weekly-hours])
; A Punchcard is a structure
; (make-punchcard Number Number)
; interpretation (make-punchcard e wh)
; e is the id, wh is the amount of hours worked this week


(define-struct wage [name weekly-pay])
; A Wage is a structure
; (make-wage String Number)
; interpretation (make-wage n wp)
; n is the employee name, wp is the amount of money they get
; this week
  
; Number Number -> Number
; computes the weekly wage from pay-rate and hours-worked
(define (weekly-wage pay-rate hours-worked)
  (* pay-rate hours-worked))

; [List-of Employee] [List-of Punchcard] -> [List-of Wage]
; get the wages of employees from the employee
; list and punchcards

(check-expect
 (wages*.v3 (list
             (make-employee "bob" 1 5.0)
             (make-employee "bobby" 2 7.0))
            (list
             (make-punchcard 1 40)
             (make-punchcard 2 42)))
 (list
  (make-wage "bob" 200.0)
  (make-wage "bobby" 294.0)))
(check-expect
 (wages*.v3 '()
            (list
             (make-punchcard 1 40)
             (make-punchcard 2 42))) '())
(check-expect
 (wages*.v3 (list
             (make-employee "bob" 1 5.0)
             (make-employee "bobby" 2 7.0))
            '())
 '())
(check-expect (wages*.v3 '() '()) '())
             

(define (wages*.v3 loe lop)
  (local ((define (get-employee-hours emp)
            (first (filter
                    (lambda (p)
                      (if (= (employee-eid emp)
                             (punchcard-eid p))
                          #true #false))
                    lop))))
    (if (empty? lop) '()
        (map (lambda (e)
               (make-wage
                (employee-name e)
                (weekly-wage
                 (employee-pay-rate e)
                 (punchcard-weekly-hours
                  (get-employee-hours e)))))
             loe))))

; [List-of Number] [List-of Numbers] -> Number
; produces the combination of the
; linear combinations l0 and the values in l1

(check-expect (value '(5 17 3) '(10 1 2)) 73)
(check-expect (value '(0 0 0) '(10 1 2)) 0)

(define (value l0 l1)
  (cond
    [(empty? l0) 0]
    [else
     (+
      (* (first l0) (first l1))
      (value (rest l0) (rest l1)))]))


; [List Symbol] [List-of Symbol] -> Boolean
; the pattern is matched to the search string, #t if match

(check-expect (DNAprefix '(a c g t t) '(a c g t t c))
              #t)
(check-expect (DNAprefix '(a c g t t) '(a c g a t t c))
              #f)
(check-expect (DNAprefix '() '(a c g a t t c))
              #t)
(check-expect (DNAprefix '(a c g t t) '())
              #f)
(check-expect (DNAprefix '() '())
              #t)

(define (DNAprefix pattern search)
  (cond
    [(empty? pattern) #t]
    [(empty? search) #f]
    [else
     (and
      (symbol=? (first pattern) (first search))
      (DNAprefix (rest pattern) (rest search)))]))

; [List-of Symbol] [List-of Symbol] -> [Maybe Symbol]
; get first item in search beyond the pattern.
; if identical lists then error
; if pattern doesn't match then #f

(check-error (DNAdelta '(a c g t t) '(a c g t t))
             "pattern matches search")
(check-expect (DNAdelta '(a c g t t) '(a c g t t c))
              'c)
(check-expect (DNAdelta '(a c g t t) '(a c g a t t c))
              #f)
(check-expect (DNAdelta '() '(a c g a t t c))
              'a)
(check-expect (DNAdelta '(a c g t t) '())
              #f)
(check-error (DNAdelta '() '())
             "pattern matches search")

(define (DNAdelta pattern search)
  (cond
    [(and
      (empty? pattern) (empty? search))
     (error "pattern matches search")]
    [(and (empty? pattern) (cons? search)) (first search)]
    [(and (cons? pattern) (empty? search)) #f]
    [(and (cons? pattern) (cons? search))
     (if (not (symbol=? (first pattern) (first search)))
         #f
         (DNAdelta (rest pattern) (rest search)))]))

; An S-expr is one of:
; - Atom
; - [List-of S-expr]

; An Atom is one of:
; - Number
; - String
; - Symbol

; Any -> Boolean
(define (atom? x)
  (cond [(number? x) #t]
        [(string? x) #t]
        [(symbol? x) #t]
        [else #f]))

; S-expr S-expr -> Boolean
; are s0 and s1 the same?

(check-expect
 (s-expr=? '() '(s (f r))) #f)
(check-expect
 (s-expr=? '() '()) #t)
(check-expect
 (s-expr=? '(s (f r)) '()) #f)
(check-expect
 (s-expr=? '(s (f r)) '(s (f r))) #t)
(check-expect
 (s-expr=? '(s (f s r)) '(s (f r))) #f)
(check-expect
 (s-expr=? 'a '(s (f r))) #f)
(check-expect
 (s-expr=? '(s (f r)) 'b) #f)
(check-expect
 (s-expr=? 'a 'a) #t)

(define (s-expr=? s0 s1)
  (cond
    [(and (empty? s0) (empty? s1)) #t]
    [(and (atom? s0) (atom? s1))
     (equal? s0 s1)]
    [(and (cons? s0) (cons? s1))
     (and
      (equal? (first s0) (first s1))
      (s-expr=? (rest s0) (rest s1)))]
    [else #f]))

; 577






















