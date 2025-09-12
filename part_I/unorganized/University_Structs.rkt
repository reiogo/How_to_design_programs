;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname University_Structs) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct student [f-name l-name gpa])
; Student is a structure:
; (make-student String String Float)
; interpretation: (make-student f l g)
; f is the first name
; l is the last name
; g is the gpa
(define (stu-template s)
  (... (student-f-name s)
       (student-l-name s)
       (student-gpa s)
       ))

(define-struct professor [f-name l-name tenure])
; Professor is a structure:
; (make-professor String String Boolean)
; interpretation: (make-professor f l t)
; f is the first name
; l is the last name
; t is the tenure status,
; - true means tenured
; - false means not tenured
(define (prof-template p)
  (... (professor-f-name p)
       (professor-l-name p)
       (professor-tenure p)))

(define-struct staff [f-name l-name salary])
; Staff is a structure
; (make-staff String String Number)
; interpretation: (make-staff f l s)
; f is first name
; l is last name
; s is the salary group
; s is one of:
; - early 
; - mid
; - senior

(define (staff-template s)
  (... (staff-f-name s)
       (staff-l-name s)
       (cond
         [(string=? (staff-salary s) "early") ...]
         [(string=? (staff-salary s) "early") ...]
         [(string=? (staff-salary s) "early") ...]
         )))
  