;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname predicates_sigs_coord_vanim) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; SIGS
; Coordinate
; VAnimal

(define-struct vcat [x happiness direction])
(define-struct vcham [x happiness direction color])
; A VAnimal is one of:
; – a VCat (make-vcat X-Pos Happiness VCatDirection)
; – a VCham (make-vcham X-Pos Happiness Direction Color)

; Any -> Boolean
; is a an element of the VAnimal collection
(check-expect (VAnimal? (make-vcat 4 4 -5))
              #true)
(check-expect (VAnimal? (make-vcham 4 4 -5 "green"))
              #true)
(check-expect (VAnimal? "hi")
              #false)
(check-expect (VAnimal? 4)
              #false)
(check-expect (VAnimal? (circle 4 "solid" "red"))
              #false)

(define (VAnimal? a)
  (cond
    [(vcat? a) #true]
    [(vcham? a) #true]
    [else #false]))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
(define-struct tank [loc vel])
; A SIGS is one of:
; - (make-aim UFO TANK)
; - (make-fired UFO Tank Missile)
; interpretation represents the state of the space invader game

; Any -> Boolean
; is a an element of the SIGS collection?
(check-expect
 (SIGS?
  (make-aim
   (make-posn 4 5)
   (make-tank 4 5)))
 #true)
(check-expect
 (SIGS?
  (make-fired
   (make-posn 4 5)
   (make-tank 4 5)
   (make-posn 4 5)))
 #true)
(check-expect
 (SIGS?
  "hi")
 #false)
(check-expect
 (SIGS?
  10)
 #false)
(check-expect
 (SIGS?
  (circle 3 "solid" "red"))
 #false)

(define (SIGS? a)
  (cond [(aim? a) #true]
        [(fired? a) #true]
        [else #false]))

; A Coordinate is one of:
; – a NegativeNumber
; – a PositiveNumber
; – a Posn

; Any -> Boolean
; is a an element of the Coordinate collection.
(check-expect (coordinate? -1) #true)
(check-expect (coordinate? 1) #true)
(check-expect (coordinate? (make-posn 1 1)) #true)
(check-expect (coordinate? "hi") #false)
(check-expect (coordinate? (empty-scene 100 100)) #false)

(define (coordinate? a)
  (cond
    [(number? a) #true]
    [(posn? a) #true]
    [else #false]))
