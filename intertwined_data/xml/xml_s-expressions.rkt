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


(define SIZE 12) ; font size
(define COLOR "black") ; font-color
(define BULLET ; a graphical constant
  (beside (circle 1 "solid" "black") (text " " SIZE COLOR)))

; Image -> Image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BULLET item))



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

(define en0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define item1-rendered
  (beside/align 'center BULLET (text "one" 12 'black)))
(define item2-rendered
  (beside/align 'center BULLET (text "two" 12 'black)))
(define en0-rendered
  (above/align 'left item1-rendered item2-rendered))

; XItem.v1 -> Image
; renders a single item as a "word" prefixed by a bullet

(check-expect
 (render-item1 (cons 'li (cons '(word ((text "two"))) '())))
 (beside/align 'center BULLET (text "two" 12 'black)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define word-in-i (word-text element)))
    (beside/align 'center BULLET (text word-in-i 12 'black))))

; XEnum.v1 -> Image
; renders a simple enumeration as an image

(check-expect (render-enum1 en0) en0-rendered)

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one-item fst-itm so-far)
            (above/align 'left (render-item1 fst-itm) so-far)))
    (foldr deal-with-one-item empty-image content)))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image

(check-expect (render-enum en0) en0-rendered)

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ;XItem.v2 Image -> Image
          (define (deal-with-one-item fst-itm so-far)
            (above/align 'left (render-item fst-itm) so-far)))
    (foldr deal-with-one-item empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image

(check-expect (render-item `(li ,en0)) (bulletize en0-rendered))

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (beside/align
     'center BULLET
     (cond
       [(word? content) (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))


; XEnum.v2 -> Image
; make bullet points from xe

(check-expect (my-render-enum en0)
              en0-rendered)

(define (my-render-enum xe)
  (cond
    [(not (list-of-attributes? (second xe)))
     (my-render-items (rest xe))]
    [else
     (my-render-items (rest (rest xe)))]))

; [List-of XItem.v2] -> Image
; make bullet points from l

(check-expect (my-render-items (list '(li (word ((text "one"))))
                                     '(li (word ((text "two"))))))
              en0-rendered)

(define (my-render-items l)
  (cond
    [(empty? l) empty-image]
    [else
     (above/align 'left (my-render-item (first l))
                  (my-render-items (rest l)))]))

; XItem.v2 -> Image
; make a bullet point from it

(check-expect (my-render-item
               '(li (word ((text "one")))))
              (beside/align 'center BULLET (text "one" 12 'black)))

(define (my-render-item it)
  
  (cond
    [(word? (second it))
     (beside/align 'center BULLET
                   (text (word-text (second it)) SIZE 'black))]
    [(list-of-attributes? (second it))
     (cond
       [(word? (third it))
        (beside/align 'center BULLET
                      (text (word-text (third it)) SIZE 'black))]
       [else (my-render-enum (third it))])]
    [else (my-render-enum (second it))]))


; XEnum.v2 -> Number
; counts all occurences of "hello" in a given XEnum.v2

(check-expect
 (count-hello-enum
  '(ul (li (word ((text "one")))))) 0)
(check-expect
 (count-hello-enum
  `(ul ,a0
       (li (word ((text "one"))))
       (li (word ((text "hello")))))) 1)

(check-expect
 (count-hello-enum
  `(ul
    (li (word ((text "hello"))))
    (li (ul ,a0
            (li (word ((text "one"))))
            (li (word ((text "hello")))))))) 2)

(define (count-hello-enum xen)
  (local ((define content (xexpr-content xen))
          ; XItem.v2 Number -> Number
          (define (deal-with-one-item fst-itm so-far)
            (+ (count-hello-item fst-itm) so-far))
          )
    (foldr deal-with-one-item 0 content)))

; XItem.v2 -> Number
; counts the number of "hello"s in xit

(check-expect (count-hello-item `(li (word ((text "hello")))))
              1)
(check-expect (count-hello-item `(li (ul ,a0
                                         (li (word ((text "one"))))
                                         (li (word ((text "hello"))))))) 1)


(define (count-hello-item xit)
  (local ((define content (first (xexpr-content xit))))
    (cond
      [(word? content)
       (if
        (string=? (word-text content) "hello")
        1 0)]
      [else (count-hello-enum content)])))

; XEnum.v2 -> XEnum.v2
; replace all "hello"s with "bye"s in xe

(check-expect
 (sub-hello-enum `(ul
                   (li (word ((text "hello"))))
                   (li (ul ,a0
                           (li (word ((text "one"))))
                           (li (word ((text "hello"))))))))
 `(ul
   (li (word ((text "bye"))))
   (li (ul ,a0
           (li (word ((text "one"))))
           (li (word ((text "bye"))))))))
(check-expect
 (sub-hello-enum `(ul ,a0
                      (li (word ((text "hello"))))
                      (li (ul ,a0
                              (li (word ((text "one"))))
                              (li (word ((text "hello"))))))))
 `(ul ,a0
      (li (word ((text "bye"))))
      (li (ul ,a0
              (li (word ((text "one"))))
              (li (word ((text "bye"))))))))

(define (sub-hello-enum xe)
  (local ((define content (xexpr-content xe)))
    (cons 'ul
          (if
           (list-of-attributes? (second xe))
           (cons (second xe) (map sub-hello-item content))
           (map sub-hello-item content)))))

; XItem.v2 -> XItem.v2
; replace all "hello"s with "bye"s in xit

(check-expect
 (sub-hello-item '(li (word ((text "hello")))))
 '(li (word ((text "bye")))))
(check-expect
 (sub-hello-item `(li ,a0 (word ((text "hello")))))
 `(li ,a0 (word ((text "bye")))))
(check-expect
 (sub-hello-item `(li ,a0 (word ((text "hell")))))
 `(li ,a0 (word ((text "hell")))))


(define (sub-hello-item xit)
  (cons 'li
        (local ((define content (rest xit))
                ; XWord -> XWord
                ; replace (if) hello with bye
                (define (replace-w-hello xw)
                  (if (string=? (word-text xw) "hello")
                      '(word ((text "bye")))
                      xw)))
          (cond
            [(list-of-attributes? (first content))
             (cond
               [(word? (second content))
                (cons
                 (first content)
                 (list (replace-w-hello (second content))))]
               [else
                (cons
                 (first content)
                 (list (sub-hello-enum (second content))))])]
            [(word? (first content))
             (list (replace-w-hello (first content)))]
            [else
             (list (sub-hello-enum (first content)))]))))

    










