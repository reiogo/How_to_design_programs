;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml_s-expr_definitions) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; An Xexpr.v0 (short for X-expression) is a list of one item:
; (cons Symbol '())

; An Xexpr.v1 is a list :
; (cons Symbol [List-of Xexpr.v1])

; A Xexpr.v2 is a one of:
; - (cons Symbol [List-of X-expr.v2])
; - (cons Symbol
;         (cons [list-of Attribute] [List-of Xexpr.v2]))

; An Attribute is a list of two items:
; (cons Symbol (cons String '()))

; ===================================================
; ALTERNATIVE DEFINITION
; A Xexpr.v2 is a one of:
; - (cons Symbol LXexpr)
; - (cons Symbol
;         (cons LAttribute LXexpr))

; A LXexpr is one of:
; - '()
; - (cons Xexpr.v2 LXexpr)

; An LAttribute is a one of:
; - '()
; - (cons Attribute LAttribute)
; ===================================================

; A Xexpr.v2 is one of:
; - XWord
; - (cons Symbol [List-of Xexpr.v2])
; - (cons Symbol (cons [list-of Attribute] [List-of Xexpr.v2]))

; An XWord is '(word ((text String))).

; Loa/xe is one of:
; - [List-of Attribute]
; - Xexpr.v2

; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v1]))

; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))

; An XEnum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

; An XItem.v2 is one of:
; - (cons 'li (cons XWord '())
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

