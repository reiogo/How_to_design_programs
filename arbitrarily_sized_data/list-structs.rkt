;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-structs) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
; (make-work String Number Number)
; interpretation (make-work n r h) combines the name (n)
; with the pay rate (r) and the number of hours (h) worked.

; Low (short for list of works) is one of:
; - '()
; - (cons Work Low)
; interpretation an instance of Low represents the hours worked
; for a number of employees
; '()
;
; (cons (make-work "Robby" 11.95 39) '())
;
; (cons (make-work "Roi" 149 30)
;   (cons (make-work "pauvre" 2 45) '()))
;
; (cons (make-work "ri" 12.34 49)
;  (cons (make-work "rie" 14 39)
;    (cons (make-work "riii" 14 34) '())))

; Number -> Number
; computes the wage for h hours of work with a rate of r
(define (payhour r h)
  (* r h))

; Low -> List-of-numbers
; computes the weekly wages for all given weekly work records
(check-expect
 (wage*.v2 (cons (make-work "Roi" 149 30)
                 (cons (make-work "pauvre" 2 45) '())))
 (cons (payhour 149 30) (cons (payhour 2 45) '())))
  
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons
      (payhour
       (work-rate (first an-low))
       (work-hours (first an-low)))
      (wage*.v2 (rest an-low)))]))

; Work -> ???
; a template for functions that process work structures
(define (for-work w)
  (... (work-employee w) (work-rate w) (work-hours w)))

(define-struct paycheck [employee amount])
; Paycheck is a structure:
; (make-paycheck String Number)
; interpretation, (make-paycheck e a)
; a paycheck is the paycheck of an amount(a)
; of money for an employee (e)

; A list-of-paychecks is one of:
; - '()
; (cons paycheck list-of-paychecks)
; interpretation a list of paychecks

; Work -> Number
; computes the paycheck for the given work record w
(define (work-to-paycheck w)
  (make-paycheck
   (work-employee w)
   (* (work-rate w) (work-hours w))))



; List-of-work -> list-of-paychecks
; computes a list of paychecks from a list of work records
(check-expect (wage*.v3 '()) '())
(check-expect
 (wage*.v3
  (cons
   (make-work "rob" 13.40 39) '()))
 (cons
  (make-paycheck "rob" (payhour 13.40 39))
  '()))
(define (wage*.v3 alow)
  (cond
    [(empty? alow) '()]
    [(cons? alow)
     (cons (work-to-paycheck (first alow)) (wage*.v3 (rest alow)))]))


(define-struct id-employee [id rate hours])
; id-employee is a structure
; (make-id-employee Number Number Number)
; interpretation (make-id-employee i r h)
; employee id (i) paid rate (r) and hours worked (h)


; Work -> Number
; computes the paycheck for the given work record w
(define (id-employee-to-paycheck w)
  (make-paycheck
   (id-employee-id w)
   (* (id-employee-rate w) (id-employee-hours w))))


; list-of-id-employees -> list-of-id-paychecks
; compute a list of paychecks from list-of-id-employees
(check-expect
 (wage*.v4
  (cons (make-id-employee 5 12 40) '()))
 (cons (make-paycheck 5 (* 12 40)) '()))
(define (wage*.v4 alow)
  (cond
    [(empty? alow) '()]
    [(cons? alow)
     (cons
      (id-employee-to-paycheck (first alow))
      (wage*.v4 (rest alow)))]))

; A list-of-posns is one of:
; - '()
; - (cons Posn list-of-posns)
; interpretation a list of posns

; list-of-posns -> Number
; sums the x-coordinates in a list of posns
(check-expect
 (sum '())
 0)
(check-expect
 (sum (cons (make-posn 3 4) '()))
 3)
(check-expect
 (sum (cons (make-posn 3 4) (cons (make-posn 4 4) '()))) 7)
(define (sum alop)
  (cond
    [(empty? alop) 0]
    [(cons? alop)
     (+ (posn-x (first alop)) (sum (rest alop)))]))

; posn -> posn
; add 1 to y
(check-expect (add-to-y (make-posn 3 5)) (make-posn 3 6))
(define (add-to-y p)
  (make-posn (posn-x p) (add1 (posn-y p))))

; list-of-posns -> list-of-posns
; translate the positions of alop by one in the y coordinate
(check-expect (translate
               (cons (make-posn 4 5)
                     (cons (make-posn 21 49) '())))
              (cons (make-posn 4 6)
                    (cons (make-posn 21 50) '())))
(define (translate alop)
  (cond
    [(empty? alop) '()]
    [(cons? alop)
     (cons (add-to-y (first alop))
           (translate (rest alop)))]))


; Posn -> Boolean
; check if p is legal
(check-expect (legal? (make-posn 1 2)) #true)
(check-expect (legal? (make-posn -2 299)) #false)
(check-expect (legal? (make-posn 12 399)) #false)
(check-expect (legal? (make-posn -2 99)) #false)
(define (legal? p)
  (cond
    [(and
      (< 0 (posn-x p) 100)
      (< 0 (posn-y p) 200)) #true]
    [else #false]))


; list-of-posns -> list-of-posns
; filter by x is between 0 and 100 and y is between 0 and 200
(check-expect (legal (cons (make-posn 3 -4)
                           (cons (make-posn -1 203)
                                 (cons (make-posn 3 102) '()))))
              (cons (make-posn 3 102) '()))
(define (legal alop)
  (cond
    [(empty? alop) '()]
    [(cons? alop)
     (cond
       [(legal? (first alop))
        (cons (first alop) (legal (rest alop)))]
       [else (legal (rest alop))])]))

(define-struct phone [area switch four])
; A Phone is a structure
; (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Fours is a Number between 1000 and 9999.

; A list-of-phones is one of:
; - '()
; - (cons Phone List-of-phones)
; interpretation a list of phone numbers

; Phone -> Phone
; template for phones
(check-expect (replace-713
               (make-phone 713 322 330))
              (make-phone 281 322 330))
(check-expect (replace-713
               (make-phone 714 322 330))
              (make-phone 714 322 330))
(define (replace-713 p)
  (cond
    [(= 713 (phone-area p))
     (make-phone
      281
      (phone-switch p)
      (phone-four p))
     ]
    [else p]))

; REPLACE
; List-of-phones -> List-of-phones
; replaces the area code 713 with 281

(check-expect
 (replace
  (cons
   (make-phone 713 444 555)
   '()))
 (cons
  (make-phone 281 444 555) '()))
(check-expect
 (replace
  (cons
   (make-phone 313 333 333)
   (cons
    (make-phone 713 444 555)
    '())))
 (cons
  (make-phone 313 333 333)
  (cons
   (make-phone 281 444 555) '())))


 (define (replace alop)
   (cond
     [(empty? alop) '()]
     [(cons? alop)
      (cons (replace-713 (first alop)) (replace (rest alop)))]))













 