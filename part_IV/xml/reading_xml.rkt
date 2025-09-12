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

; XML PROCESSORS ==============================================

(define a0 '((initial "red")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Xexpr.v2]
; get the content of xe

(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content e2) '((action)))

(define (xexpr-content xe)
  (if
   (list-of-attributes? (second xe))
   (rest (rest xe))
   (rest xe)))

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
; find attribute in loa using s as the key

(check-expect (lookup-attribute a0 'initial) "red")
(check-expect (lookup-attribute a0 'init) #f)

(define (lookup-attribute loa s)
  (local ((define pair (assq s loa)))
    (if (cons? pair)
        (second pair)
        #f
        )))

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
; =============================================================

; two pieces of a URL
(define PREFIX "https://ww.google.com/finance?q=")
(define SUFFIX "&btnG=Search")
(define SIZE 22) ; font size
(define SPACER (text " " SIZE 'white)) ; a graphical image

(define-struct data [price delta])
; A StockWorld is a structure:
; (make-data String String)
; interpretation in (make-data p d), p and d
; specify the current price and how much it changes since the last update

; String -> StockWorld
; retrieves stock price and its change of the specified company
; every 15 seconds and displays together with available time stamp
(define (stock-alert company)
  (local ((define url (string-append PREFIX company SUFFIX))

          ; [StockWorld -> StockWorld]
          ; retrieve price and change from url
          (define (retrieve-stock-data _w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price") (get x "priceChange"))))

          ; StockWorld -> Image
          ; renders the stock market data as a single long line
          (define (render-stock-data w)
            (local ((define pt (text (data-price w) SIZE 'black))
                    (define dt (text (data-delta w) SIZE 'red)))
              (overlay (beside pt SPACER dt)
                       (rectangle 300 35 'solid 'white)))))
    ; - IN -
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data]
      [name company])))


; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute for
; a 'meta element with attribute "itemprop" and value s

(check-expect
 (get '(meta ((content "+0.11") (itemprop "delta"))) "delta")
 "+0.11")
(check-expect
 (get '(meta ((itemprop "price") (content "17.01"))) "price")
 "17.01")
(check-error
 (get '(meta ((itemprop "price") (content "17.01"))) "delta")
 "attribute not found: delta")

(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error (string-append "attribute not found: " s)))))

; Xexpr.v3 String -> [Maybe String]
; search xe for an attribute s and return the corresponding string

(check-expect
 (get-xexpr '(meta ((content "a") (itemprop "d"))) "d") "a")
(check-expect
 (get-xexpr '(meta ((content "a") (itemprop "d"))) "b") #false)

(define (get-xexpr xe s)
  (local ((define loa (xexpr-attributes xe)))
  (cond
    [(empty? loa) #false]
    [else
     (if
      (string=? (lookup-attribute loa 'itemprop) s)
      (lookup-attribute loa 'content)
      #false)])))


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












