;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname poetry) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Poetry
(define-struct child [father mother name date eyes])
; A Chiild is a structure:
; (make-child Child Child String N String)

; (define Adam (make-child Carl Bettina "Adam" 1950 "yellow"))

(define-struct no-parent [])
(define MTFT (make-no-parent))
(make-child (make-no-parent) (make-no-parent) "Bettina" 1926 "green")

; A FT (short for family tree) is one of:
; - MTFT
; - (make-child FT FT String N String)

(make-child MTFT MTFT "Carl" 1926 "green")

(make-child (make-child MTFT MTFT "Carl" 1926 "green")
            (make-child MTFT MTFT "Bettina" 1926 "green")
            "Adam"
            1950
            "hazel")

; Oldest
(define Carl (make-child MTFT MTFT "Carl" 1926 "green"))
(define Bettina (make-child MTFT MTFT "Bettina" 1926 "green"))

; Middle
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1965 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child MTFT MTFT "Fred" 1966 "pink"))

; Youngest
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; FT -> ???
; ...
(define (func-for-FT a-ftree)
  (cond
    [(no-parent? a-ftree) ...]
    [else
     (... (func-for-FT (child-father a-ftree))
          (func-for-FT (child-mother a-ftree))
          (child-name a-ftree)
          (child-date a-ftree)
          (child-eyes a-ftree))]))

; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field

(check-expect (blue-eyed-child? Carl) #f)
(check-expect (blue-eyed-child? Gustav) #t)
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #f]
    [else
     (or
      (string=? (child-eyes a-ftree) "blue")
      (blue-eyed-child? (child-father a-ftree))
      (blue-eyed-child? (child-mother a-ftree))
      )]))

; FT -> Number
;how many people in the ft
(check-expect (count-persons MTFT) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [else (+ 1
             (count-persons (child-father ft))
             (count-persons (child-mother ft)))]))


; FT Number => Number
; average age of all children in ft

(check-expect (average-age MTFT 2025) 0)

(check-expect (average-age Carl 2025) 99)

(check-expect (average-age Dave 2025) (/ (+ (- 2025 1965) (- 2025 1926) 99) 3))

(define (average-age ft year)
  (cond
    [(no-parent? ft) 0]
    [else (local (; FT -> Number
                  ; get the sum of all ages
                  (define (sum-age anft)
                    (cond
                      [(no-parent? anft) 0]
                      [else (+
                             (- year (child-date anft))
                             (sum-age (child-mother anft))
                             (sum-age (child-father anft)))])))
            ; -- IN --
            (/ (sum-age ft) (count-persons ft) ))]))

; FT => [List-of String]
; get a list of all the eye colors from ft

(check-expect (eye-colors Carl) '("green"))

(check-expect (eye-colors Gustav)
              '("brown" "blue" "green" "green" "pink"))

(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [else
     (append
      (list (child-eyes ft))
      (eye-colors (child-mother ft))
      (eye-colors (child-father ft)))
     ]))


; FT -> Boolean
; check if any of the ancestors of a-ftree has blue eyes

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else (or
           (blue-eyed-child? (child-father a-ftree))
           (blue-eyed-child? (child-mother a-ftree)))]))


; A FF (short for family forest) is a [List-of FT]

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))


; [List-of FT] -> Boolean
; does the forest contain any child node with "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #f)
(check-expect (blue-eyed-child-in-forest? ff2) #t)
(check-expect (blue-eyed-child-in-forest? ff3) #t)

(define (blue-eyed-child-in-forest-original? a-forest)
  (cond
    [(empty? a-forest) #f]
    [else (or (blue-eyed-child? (first a-forest))
              (blue-eyed-child-in-forest? (rest a-forest)))]))

(define (blue-eyed-child-in-forest? a-forest)
  (ormap (lambda (c) (blue-eyed-child? c)) a-forest))

; FT -> Number
; get the sum of all ages
(define (sum-age-tree anft year)
  (cond
    [(no-parent? anft) 0]
    [else (+
           (- year (child-date anft))
           (sum-age-tree (child-mother anft) year)
           (sum-age-tree (child-father anft) year))]))

; [List-of FT] Number -> Number
; average age of non-overlapping forest

(check-expect (average-forest-age ff1 2025) (/ (+ 99 99) 2))
(check-expect (average-forest-age ff2 2025) (/ (+ 60 59 99 99) 4))
(check-expect (average-forest-age ff3 2025) (/ (+ 60 99 99 59 99) 5))

(define (average-forest-age a-forest year)
  (/
   (foldr (lambda (c f) (+ (sum-age-tree c year) f)) 0 a-forest)
   (foldr (lambda (c f) (+ (count-persons c) f)) 0 a-forest))
  )


















