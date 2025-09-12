;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname zoo_structs) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct spiders [legs space])
; Spiders is a structure:
; (make-spiders Number Size)
; interpretation (make-spiders n s)
; n is the number of remaining legs
; s is a Size of the spiders cage for transportation

; Size is one of:
; - s for small
; - m for mid
; - l for large
; interpretation: the relative size of a cage

(define elephants space)
; Elephants is a Size:
; interpretation: Size of an elephants cage for transportation

(define-struct boa-constrictor [length girth space])
; Boa-constrictor is a structure:
;(make-boa-constrictor Number Number)
;interpretation (make-boa-constrictor l g s)
; l is the length, g is the length around the thickest part
; s is the size of the storage space the snake needs

(define-struct armadillo [type space])
; Armadillo is a structure:
;(make-armadillo Armadillo-Species Size)
; interpretation (make-armadillo t s)
; t is the type of armadillo, s is the size of the transport container

;Armadillo-Species is a String and is one of:
; - "nine" which is the nine banded armadillo
; - "giant" which is the giant armadillo
; - "six" which is the six banded armadillo

; Animals is one of:
; - spiders
; - elephants
; - boa-constrictor
; - armadillo
; interpretation animals of the zoo.

; Animals Size -> Boolean
; Check whether the animal will fit the size of the cage
(define (fits? a s)
  (...cond
   [(spiders? a) (... (spider-space a) s)]
   [(elephants? a) (... (elephants-space a) s)]
   [(boa-constrictor? a) (... (boa-constrictor-space a) s)]
   [(armadillo? a) (... (armadillo-space a) s)]))
