;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nature_of_numbers) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; The Nature of Numbers

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
; (make-inex N99 S N99)

; An S is one of :
; - 1
; - -1

; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else
     (error 'inex "(<= 0 m 99), s in {+1, -1}, (<= 0 e 99) expected")]))

; Inex -> Number
; convert an index into its numeric equalivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt 10 (* (inex-sign an-inex )( inex-exponent an-inex)))))

(define eg0 (create-inex 5 -1 19))
(define eg1 (create-inex 50 -1 20))
(define eg2 (create-inex 12 1 2))
(define eg3 (create-inex 13 1 2))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

(check-expect (inex->number eg0) (inex->number eg1))
;(inex->number eg3)


; Inex Inex -> Inex
; add to Inex numbers together

(check-expect
 (inex+ (create-inex 5 -1 1) (create-inex 5 -1 1))
 (create-inex 10 -1 1))
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
 (create-inex 3 1 0))
(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))
(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))
(check-error
 (inex+ (create-inex 56 1 99) (create-inex 56 1 99))
 "out of bounds")

(define (inex+ i0 i1)
  (local ((define sum
            (+ (inex-mantissa i0) (inex-mantissa i1)))
          (define (checked-expt n)
            (if (and (= n 99) (>= sum 100))
                (error "out of bounds")
                (if
                 (<= sum 100) (inex-exponent i0)
                 (add1 (inex-exponent i0))))))
    (create-inex
     (if (>= sum 100) (round (/ sum 10)) sum)
     (inex-sign i0)
     (checked-expt (inex-exponent i0))
     )))

; Inex Inex -> Inex
; multiply Inex numbers together

(check-expect (inex* (create-inex 2 1 4)
                     (create-inex 8 1 10))
              (create-inex 16 1 14))
(check-expect (inex* (create-inex 20 1 1)
                     (create-inex 5 1 4))
              (create-inex 10 1 6))
(check-error (inex* (create-inex 20 1 49)
                    (create-inex 5 1 50))
             "out of bounds")
;(check-expect (inex* (create-inex 27 -1 1)
;                     (create-inex 7 1 4))
;              (create-inex 19 1 4))

(define (inex* s0 s1)
  (local ((define product (* (inex-mantissa s0) (inex-mantissa s1)))
          (define expt (+ (inex-exponent s0) (inex-exponent s1))))
    (create-inex
     (if (>= product 100) (/ product 10) product)
     (inex-sign s0)
     (if (>= product 100)
         (if (>= expt 99)
             (error "out of bounds") (add1 expt))
         expt)
     )))

; N -> Number
; add n copies of #i1/185

(check-expect (add-inexact 0) 0)
(check-within (add-inexact 1) #i1/185 0.0001)

(define (add-inexact n)
  (cond
    [(zero? n) 0]
    [else
     (+
      #i1/185
      (add-inexact (sub1 n)))]))

; N -> Number
; count how often 1/185 can be subtracted from n

(check-expect (sub-inexact 0) 0)
(check-expect (sub-inexact 1/185) 1)

(define (sub-inexact n)
  (cond
    [(<= n 0) 0]
    [else
     (add1 (sub-inexact (- n 1/185)))
     ]))

; N -> Number
; determine number that is an exponent of #i10.0 and is
; still an inexact number, but 1 minus the number is
; approximated to zero

(define (underflow n)
  (cond
    [(= (expt #i10.0 (sub1 n)) 0) n]
    [else
     (underflow (sub1 n))]))
; (underflow 1)
; -323

; N -> Number
; determine number that is 

(define (overflow n)
  (cond
    [(= (expt #i10.0 (add1 n)) +inf.0) n]
    [else
     (overflow (add1 n))]))
;(overflow 1)

(define (over-under f v n)
  (cond
    [(= (expt #i10.0 (f n)) v) n]
    [else
     (over-under f v (f n)) ]))

; (over-under add1 +inf.0 1)
; (over-under sub1 0 1)

;(expt 1.001 1e-12)
;(expt 1.001 1e-3)
;(expt 1.001 1/1000)
;(expt 1.001 0.01)
;(expt 1.001 0.1)
;(expt 1.001 10)
;(expt 1.001 100)
;(expt 1.001 1/10)

; Number N -> Number
; my own implementation of expt

(check-expect (my-expt 2 3) 8)
(check-expect (my-expt 5 0) 1)

(define (my-expt n pow)
  (cond
    [(zero? pow) 1]
    [else
     (* n (my-expt n (sub1 pow)))]))

(define inex-pow (+ 1 #i1e-12))
(define exac (+ 1 1e-12))
;(my-expt inex-pow 30)
;(my-expt exac 20)

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        1.0
        #i-8e+269
        0.0
        99.0))

;(foldl + 0 JANUS)
;(foldl + 0 (reverse JANUS))
;(foldl + 0 (sort JANUS <))

;(expt 2 #i53.0)
;(foldl + 0 (list #i1.0 (expt 2 #i53.0)))
;(foldl + 0 (list #i1.0  #i1.0 (expt 2 #i53.0)))
;(exact->inexact (foldl + 0 (map inexact->exact JANUS)))
;




(expt -0.99 2)









