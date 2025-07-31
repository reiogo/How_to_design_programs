;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list_phone_contacts) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-names is one of:
; - '()
; - (cons String List-of-names)
; interpretation represents a list of names

; List-of-names -> Boolean
; determines whether "Flatt" occurs on a-list-of-names
; given '() expect false
(check-expect (contains-flatt? '()) #false)
; given a list of a single item
(check-expect (contains-flatt? (cons "Findler" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
; given a list of multiple items
(check-expect
 (contains-flatt? (cons "Mur" (cons "Fish" (cons "Find" '()))))
 #false)
(check-expect
 (contains-flatt? (cons "Mur" (cons "Flatt" (cons "Find" '()))))
 #true)
(define (contains-flatt? a-list-of-names)
  (cond [(empty? a-list-of-names) #false]
        [(cons? a-list-of-names)
         (or
          (string=? "Flatt" (first a-list-of-names))
          (contains-flatt? (rest a-list-of-names)))]))

;(contains-flatt?
 ;(cons "Fagan"
  ;     (cons "Findler"
   ;          (cons "Fisler"
    ;               (cons "Flanagan"
     ;;                    (cons "Flatt"
       ;                        (cons "Felleisen"
        ;                             (cons "Friedman" '()))))))))

; List-of-strings -> Boolean
; Determine whether a-list-of-strings contains a-string
; examples
(check-expect (contains? '() "hi" ) #false)
(check-expect
 (contains?
  (cons "hel" '()) "hi")
 #false)
(check-expect
 (contains?
  (cons "hi" '()) "hi")
 #true)

(check-expect
 (contains?
  (cons "hel"
        (cons "hii" '())) "hi")
 #false)
(check-expect
 (contains?
  (cons "hello"
        (cons "hi" '())) "hi")
 #true)
(define (contains? a-list-of-strings a-string)
  (cond
    [(empty? a-list-of-strings) #false]
    [(cons? a-list-of-strings)
     (cond
       [(string=? a-string (first a-list-of-strings)) #true]
       [else (contains? (rest a-list-of-strings) a-string)])]))

; List-of-strings -> Number
; determines how many strings are on alos
(check-expect (how-many '()) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "b" (cons "a" '()))) 2)
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else
     (+ 1 (how-many (rest alos)))]))













 