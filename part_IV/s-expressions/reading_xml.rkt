;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname reading_xml) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Reading XML
; An Xexpr.v3 is one of:
; - Symbol
; - String
; - Number
; - (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
; - (cons Symbol [List-of Xexpr.v3])

; An Attribute*.v3 is a [List-of Attribute.v3].

; An Attribute.v3 is a list of two items:
; (list Symbol String)
; interpretation: (list 'a "some text") represents a="some text"
