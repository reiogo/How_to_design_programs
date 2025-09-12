;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dictionaries) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; DICTIONARIES
(define DICTIONARY-LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-Strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or equivalently, a member? of the following list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; counts how many words start with a given letter

(check-expect
 (starts-with# "a" (list "able" "apple" "can")) 2)

(check-expect
 (starts-with# "b" (list "apple")) 0)

(define (starts-with# e dict)
  (cond
    [(empty? dict) 0]
    [else
     (+
      (if
       (string=? e (substring (first dict) 0 1))
       1 0)
      (starts-with# e (rest dict)))]))

; (starts-with# "z" DICTIONARY-AS-LIST)

(define-struct letter-counts [letter n])
; A Letter-Counts is structure
; (make-letter-counts 1String Number)
; interpretation (make-letter-counts l n)
; n is the count of the letter l

; List-of-letters Dictionary -> List-of-letter-Counts
; counts the number of letters in a given dictionary

(check-expect
 (count-by-letter (list "a") (list "apple" "aardappel"))
 (list (make-letter-counts "a" 4)))

(check-expect
 (count-by-letter '() '())
 '())

(check-expect
 (count-by-letter '() (list "apple"))
 '())

(check-expect
 (count-by-letter (list "a") '())
 (list (make-letter-counts "a" 0)))

(define (count-by-letter lol d)
  (cond
    [(empty? lol) '()]
    [else
     (cons
      (l-count-indi (first lol) d)
      (count-by-letter (rest lol) d))]))

; Letter Dictionary -> Letter-Counts
; Count how many times l shows up in d

(check-expect
 (l-count-indi
  "a" (list "apple" "atomic"))
 (make-letter-counts "a" 2))

(check-expect
 (l-count-indi
  "a" '())
 (make-letter-counts "a" 0))

(define (l-count-indi l d)
  (cond
    [(empty? d) (make-letter-counts l 0)]
    [else
     (make-letter-counts l
                         (+ (count l (first d))
                            (letter-counts-n (l-count-indi l (rest d)))))]))

; Letter String -> Number
; count how many times l shows up in s

(check-expect (count "a" "apathy") 2)

(check-expect (count "a" "pithy") 0)

(define (count l s)
  (cond
    [(zero? (string-length s)) 0]
    [else
     (+
      (if
       (string=? (substring s 0 1) l)
       1 0)
      (count l (substring s 1 (string-length s))))]))


; (count-by-letter LETTERS DICTIONARY-AS-LIST)

; Dictionary -> Letter-Count
; Find the most frequently used
; first letter of a word in a given dictionary

;(check-expect
; (most-frequent
;  (list "apple" "bee" "atomic"))
; (make-letter-counts "e" 3))
;
;(check-expect
; (most-frequent
;  '())
; (make-letter-counts "" 0))
;
;(define (most-frequent d)
;  (max-frequent (count-by-letter LETTERS d)))

; List-of-letter-counts -> letter-counts
; Finds the letter-count with the largest frequency

;(check-expect
; (max-frequent
;  (list
;   (make-letter-counts "a" 0)
;   (make-letter-counts "b" 2)
;   (make-letter-counts "c" 3)))
; (make-letter-counts "c" 3))
;
;(check-expect
; (max-frequent
;  '())
; (make-letter-counts "" 0))
; 
;(define (max-frequent lolc)
;  (cond
;    [(empty? lolc) (make-letter-counts "" 0)]
;    [else
;     (if 
;      (>
;       (letter-counts-n (first lolc))
;       (letter-counts-n (max-frequent (rest lolc))))
;      (first lolc) (max-frequent (rest lolc)))]))




; Dictionary -> Letter-Count
; Find the most frequently used
; first letter of a word in a given dictionary

(check-expect
 (most-frequent
  (list "apple" "bee" "atomic"))
 (make-letter-counts "a" 2))

(check-expect
 (most-frequent
  '())
 (make-letter-counts "" 0))

(define (most-frequent d)
  (max-frequent (letter-first LETTERS d)))

; List-of-letters Dictionary -> List-of-letter-counts
; create a list of letter-counts that counts how many
; times the letter showed up as the first letter

(check-expect
 (letter-first
  (list "a" "b")
  (list "apple" "beauty" "atomic"))
 (list
  (make-letter-counts "a" 2)
  (make-letter-counts "b" 1)))

(check-expect
 (letter-first
  (list "a" "b")
  (list "apple" "beauty" "atomic"))
 (list
  (make-letter-counts "a" 2)
  (make-letter-counts "b" 1)))

(define (letter-first lol d)
  (cond
    [(empty? lol) '()]
    [else
     (cons
      (count-first (first lol) d)
      (letter-first (rest lol) d))]))

; Letter Dictionary -> Count-letters
; in a given dictionary count which words start with l

(check-expect
 (count-first "a"
              (list "apple" "town" "abyss"))
 (make-letter-counts "a" 2))

(check-expect
 (count-first "a"
              '())
 (make-letter-counts "a" 0))

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


; List-of-letter-counts -> letter-counts
; Finds the letter-count with the largest frequency

(check-expect
 (max-frequent
  (list
   (make-letter-counts "a" 0)
   (make-letter-counts "b" 2)
   (make-letter-counts "c" 3)))
 (make-letter-counts "c" 3))

(check-expect
 (max-frequent
  '())
 (make-letter-counts "" 0))
 
(define (max-frequent lolc)
  (cond
    [(empty? lolc) (make-letter-counts "" 0)]
    [else
     (if 
      (>
       (letter-counts-n (first lolc))
       (letter-counts-n (max-frequent (rest lolc))))
      (first lolc) (max-frequent (rest lolc)))]))


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

(define (words-by-first-letter l d)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (first-dict (first l) d)
      (words-by-first-letter (rest l) d))]))

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


; Dictionary -> letter-counts
; Take a list of letters and a dictionary and find the
; most frequently used first letter

(check-expect
 (most-frequent.v2
  (list "apple" "bon" "achilles"))
 (make-letter-counts "a" 2))

(check-expect
 (most-frequent.v2
  '())
 (make-letter-counts "" 0))

(define (most-frequent.v2 d)
  (get-letter-counts (max-dict (words-by-first-letter LETTERS d))))

; LLoS -> letter-counts
; Find the list with the most entries

(check-expect
 (max-dict
  (list
   (list "apple" "adonis")
   (list "be")))
 (list "apple" "adonis"))

(check-expect
 (max-dict
  (list '()))
 '())

(define (max-dict allos)
  (cond
    [(empty? (rest allos)) '()]
    [else
     (if
      (> (length (first allos))
         (length (max-dict (rest allos))))
      (first allos)
      (max-dict (rest allos)))]))

; List-of-strings -> letter-counts
; Find the letter-counts of a list of strings that all start with the same
; letter

(check-expect
 (get-letter-counts 
  (list "apple" "addled"))
 (make-letter-counts "a" 2))

(check-expect
 (get-letter-counts 
  '())
 (make-letter-counts "" 0))

(define (get-letter-counts los)
  (cond
    [(empty? los) (make-letter-counts "" 0)]
    [else (count-first (substring (first los) 0 1) los)]))
 




(check-expect
 (most-frequent DICTIONARY-AS-LIST)
 (most-frequent.v2 DICTIONARY-AS-LIST))


















