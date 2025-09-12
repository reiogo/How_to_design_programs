;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vehicle_structures) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Vehicle is one of:
; - Automobile
; - Vans
; - Buses
; - SUVs
; - Trucks

(define-struct vehicle
  [type passengers license-plate fuel-consumption])
; A Vehicle is a structure
; (make-vehicle String Number String Number)
; interpretation (make-vehicle t p l f)
; t is one of
; - "automobile"
; - "van"
; - "bus"
; - "SUV"
; - "truck"
; p is the number of passengers it can accommodate
; l is the license plate
; f is the number of liters the vehicle consumes per kilometer.
(define (vehicle-template v)
  (cond
    [(string=? (vehicle-type v) "automobile") ...]
    [(string=? (vehicle-type v) "van") ...]
    [(string=? (vehicle-type v) "bus") ...]
    [(string=? (vehicle-type v) "SUV") ...]
    [(string=? (vehicle-type v) "truck") ...])