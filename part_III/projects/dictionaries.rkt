;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dictionaries) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Dictionaries - Abstractions
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
(define-struct letter-counts [letter n])

; MOST-FREQUENT ===============================================
; Dictionary -> Letter-Count
; Find the most frequently used
; first letter of a word in a given dictionary

(check-expect
 (most-frequent
  (list "apple" "bee" "atomic"))
 (make-letter-counts "a" 2))

(check-expect
 (most-frequent
  (list "apple" "bee" "atomic" "bean" "burrow"))
 (make-letter-counts "b" 3))

(check-expect
 (letter-counts-n (most-frequent
  '()))
 0)

(define (most-frequent dict)
  (local (; [List-of String] -> [List-of letter-counts]
          ; find the frequency of letters as the first element
          (define (letter-first-inner letters)
            (cond
              [(empty? letters) '()]
              [else
               (cons
                (make-letter-counts
                 (first letters)
                 (local (; String Number -> Number
                         ; add to n if string starts with certain letter
                         (define (add-if-letter s0 n0)
                           (if (string=? (substring s0 0 1) (first letters))
                               (add1 n0)
                               n0)))
                 (foldr add-if-letter 0 dict))
                 )
                (letter-first-inner (rest letters)))]))
          ; Letter-count Letter-count -> Boolean
          ; whether lc0 is greater than lc1
          (define (greater? lc0 lc1)
            (> (letter-counts-n lc0) (letter-counts-n lc1)))
          ; [List-of letter-counts] -> letter-counts
          (define (max-frequent-inner lolc)
            (first (sort lolc greater?))))
    ; -- IN --
    (max-frequent-inner (letter-first-inner LETTERS))))

; LETTER-FIRST ===============================================
; List-of-letters Dictionary -> List-of-letter-counts
; create a list of letter-counts that counts how many
; times the letter showed up as the first letter
(define (letter-first lol d)
  (cond
    [(empty? lol) '()]
    [else
     (cons
      (count-first (first lol) d)
      (letter-first (rest lol) d))]))

; MAX-FREQUENT ===============================================
; List-of-letter-counts -> letter-counts
; Finds the letter-count with the largest frequency
(define (max-frequent lolc)
  (cond
    [(empty? lolc) (make-letter-counts "" 0)]
    [else
     (if 
      (>
       (letter-counts-n (first lolc))
       (letter-counts-n (max-frequent (rest lolc))))
      (first lolc) (max-frequent (rest lolc)))]))

; COUNT-FIRST ===============================================
; Letter Dictionary -> Count-letters
; in a given dictionary count which words start with l

(define (count-first l d)
  (cond
    [(empty? d) (make-letter-counts l 0)]
    [else
     (make-letter-counts
      l
      (+ (if
          (string=? l (substring (first d) 0 1))
          1 0)
         (letter-counts-n (count-first l (rest d)))))]))


; WORD-BY-FIRST-LETTER ===============================================
; List-of-letters Dictionary -> list-of-dictionaries
; Creates a list of dictionaries for each letter
; where the word starts with the given letter

(check-expect
 (words-by-first-letter (list "a" "b" "c")
                        (list "apple" "bee" "crampon"))
 (list
  (list "apple")
  (list "bee")
  (list "crampon")))

(check-expect
 (words-by-first-letter
  (list "a" "b" "c")
  (list "apple" "atomic" "bee" "crampon"))
 (list
  (list "apple" "atomic")
  (list "bee")
  (list "crampon")))

(check-expect
 (words-by-first-letter
  (list "a")
  '())
 (list '()))

(define (words-by-first-letter l dict)
  (local (; 1String [List-of Dictionaries] -> [List-of Dictionaries]
          ; add dictionary of the current letter to the rest
          (define (add-dict s0 lod)
            (local (; String -> Boolean
                    ; if string starts with s0 then true
                    (define (starts-with-s0? s1)
                      (string=? (substring s1 0 1) s0))
                    )
            (cons (filter starts-with-s0? dict) lod)))
            )
  (foldr add-dict '() l)))

; FIRST-DICT ===============================================
; Letter Dictionary -> Dictionary
; creates a dictionary of the words where the first
; letter is l

(check-expect
 (first-dict "a"
             (list "apple" "add" "bent"))
 (list "apple" "add"))

(check-expect
 (first-dict "a"
             '())
 '())

(define (first-dict l d)
  (cond
    [(empty? d) '()]
    [else
     (if (string=? (substring (first d) 0 1) l)
         (cons (first d) (first-dict l (rest d)))
         (first-dict l (rest d)))]))
























