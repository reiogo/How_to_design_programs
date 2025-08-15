;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname quotes_quasiquotes) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
;Quotes and Quasiquotes
(check-expect
 `(1 "a" 2 #false 3 "c")
 (list 1 "a" 2 #false 3 "c"))

(check-expect
 `(("alan" ,(* 2 500))
   ("barb" 2000)
   (,(string-append "carl" ", the great") 1500)
   ("dawn" 2300))
 (list
  (list "alan" 1000)
  (list "barb" 2000)
  (list "carl, the great" 1500)
  (list "dawn" 2300))
 )

(define title "rating")

(check-expect
 `("html"
   ("head"
    ("title" ,title))
   ("body"
    ("h1" ,title)
    ("p" "A second web page")))
 (list "html"
       (list "head"
             (list "title" "rating"))
       (list "body"
             (list "h1" "rating")
             (list "p" "A second web page"))))

'(table ((border "1"))
        (tr (td "1") (td "2") (td "3") (td "4"))
        (tr (td "2.8") (td "-1.1") (td "3.4") (td "1.3")))

; List-of-numbers -> ... nested list
; creates a row for an HTML table from a list of numbers
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))

; Number -> ...nested list
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))

(make-cell 2)
(make-row '(1 2))

; List-of--numbers List-of-numbers -> ...nested list
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

(make-table '(1 2 3 4 5) '(3.5 2.8 -1.1 3.4 1.3))

(check-expect
 `(0 ,@'(1 2 3) 4)
 (list 0 1 2 3 4))

(check-expect
 `(0 ,@'(1 2 3) 4)
 '( 0 1 2 3 4))

(check-expect
 `(("alan" ,(* 2 500))
   ("barb" 2000)
   (,@'("carl" ", the great") 1500)
   ("dawn" 2300))
 (list
  (list "alan" 1000)
  (list "barb" 2000)
  (list "carl" ", the great" 1500)
  (list "dawn" 2300)))


(check-expect
 `(html
   (body
    (table ((border "l"))
           (tr ((width "200")) ,@(make-row '(1 2)))
           (tr ((width "200")) ,@(make-row '(99 65))))))
 '(html
   (body
    (table ((border "l"))
           (tr ((width "200")) (td "1") (td "2"))
           (tr ((width "200")) (td "99") (td "65"))))))

(check-expect
 `(html
   (body
    (table ((border "l"))
           (tr ((width "200")) ,@(make-row '(1 2)))
           (tr ((width "200")) ,@(make-row '(99 65))))))
 (list 'html
       (list 'body
             (list 'table (list (list 'border '"l"))
                   (list 'tr (list (list 'width '"200"))
                         (list 'td '"1") (list 'td '"2"))
                   (list 'tr (list (list 'width '"200"))
                         (list 'td '"99") (list 'td '"65"))))))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> html
; make a html page of a list from alos
(define (make-ranking alos)
  `(html
    (body
     (table ((border "l"))
            ,@(l-o-r alos)))))

; List-of-strings -> List-of-rows
; make a list of tables entries from alos

(check-expect
 (l-o-r one-list)
 '(
   (tr ((width "200")) "Asia: Heat of the Moment")
   (tr ((width "200")) "U2: One")
   (tr ((width "200")) "The White Stripes: Seven Nation Army")
   ))

(define (l-o-r alos)
  (cond
    [(empty? alos) '()]
    [else
     (cons 
      `(tr ((width "200")) ,(first alos))
      (l-o-r (rest alos)))]))

;(show-in-browser
 ;(make-ranking one-list))

; RS is a list where
; first is the rank, second is the song string

; List-of-strings -> RS
; create a ranked list from a los

(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> RS
; add ranks to songs from los
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))



















