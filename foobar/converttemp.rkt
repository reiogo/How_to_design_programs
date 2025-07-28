;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname converttemp) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;Use a simple Number to represent Temperature
;Number -> Number
;Convert from Fahrenheit to Celsius
(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)
(define (f2c f)
  (* 5/9 (- f 32)))

(define (convert in out)
  (write-file out
    (string-append
     (number->string
      (f2c
       (string->number
        (read-file in))))
     "\n")))
