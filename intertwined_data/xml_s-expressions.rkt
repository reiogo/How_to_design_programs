;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml_s-expressions) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
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

(define s0 '(transition ((from "seen-e") (to "seen-f"))))
(define s1 '(ul (li (word)) (li (word))))
(define s2 '(end))

; <server name="example.org" />

; <carcassone board="grass>
;  <player name="sam" />
; </carcassone>

; <start />

(define a0 '((initial "red")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Symbol]
; retrieves the list of names of xe

(check-expect (xexpr-names e0) '(machine))
(check-expect (xexpr-names e1) '(machine))
(check-expect (xexpr-names e2) '(machine action))
(check-expect (xexpr-names e3) '(machine action))
(check-expect (xexpr-names e4) '(machine action action))

(define (xexpr-names xe)
  (local ((define loa-content (rest xe))
          (define name (first xe)))
    (cond
      [(empty? loa-content) (list name)]
      [(list-of-attributes? (first loa-content))
       (cons name (lox-names (rest loa-content)))]
      [else
       (cons name (lox-names loa-content))])))

(define (lox-names l)
  (cond
    [(empty? l) '()]
    [else
     (append
      (xexpr-names (first l))
      (lox-names (rest l)))]))


; Xexpr.v2 -> Symbol
; get the name of xe

(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Xexpr.v2]
; get the content of xe

(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content e2) '((action)))

(define (xexpr-content xe)
  (if
   (list-of-attributes? (second xe))
   (rest (rest xe))
   (rest xe)))



; A Xexpr.v2 is one of:
; - XWord
; - (cons Symbol [List-of Xexpr.v2])
; - (cons Symbol
;         (cons [list-of Attribute] [List-of Xexpr.v2]))


; An XWord is '(word ((text String))).



; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attributes e0) '())
(check-expect (xexpr-attributes e1) '((initial "red")))
(check-expect (xexpr-attributes e2) '())
(check-expect (xexpr-attributes e3) '())
(check-expect (xexpr-attributes e4) '((initial "red")))

(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; Loa/xe is one of:
; - [List-of Attribute]
; - Xexpr.v2

; Loa/xe -> Boolean
; deteremine whether x is an element of [List-of Attribute]
; #false otherwise
(check-expect (list-of-attributes? a0) #t)
(check-expect (list-of-attributes? e0) #f)
(check-expect (list-of-attributes? e4) #f)

(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; [List-of Attribute] Symbol -> [Maybe String]

(check-expect (lookup-attribute a0 'initial) "red")
(check-expect (lookup-attribute a0 'init) #f)


(define (lookup-attribute loa s)
  (local ((define pair (assq s loa)))
    (if (cons? pair)
        (second pair)
        #f
        )))



(define xw0 '(word ((text "hi"))))
(define xw1 '(word ((text "hello")(text "hi"))))

; Any -> Boolean
; is a an xword?

(check-expect (word? xw0) #t)
(check-expect (word? "hi") #f)

(define (word? a)
  (if (and (list? a) (symbol=? (first a) 'word))
      #t #f))

; XWord -> String
; extracts text from xw

(check-expect (word-text xw0) "hi")

(define (word-text xw)
  (second (first (second xw))))

; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v1]))

; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))

(define en0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))
(define BULLET (circle 1 "solid" "black"))

(define item1-rendered
  (beside/align 'center BULLET (text "one" 12 'black)))
(define item2-rendered
  (beside/align 'center BULLET (text "two" 12 'black)))
(define e0-rendered
  (above/align 'left item1-rendered item2-rendered))

; XItem.v1 -> Image
; renders a single item as a "word" prefixed by a buullet
(define (render-item1 i)
  (... (xexpr-content i)))
















