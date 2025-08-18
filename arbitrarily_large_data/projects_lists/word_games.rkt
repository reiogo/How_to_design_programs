;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname word_games) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
; Word Games

; String -> List-of-strings
; find all words that the letters of some given word spell

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))

(define (all-words-from-rat? w)
  (and
   (member? "rat" w)
   (member? "art" w)
   (member? "tar" w)))

(check-satisfied (alternative-words "rat") all-words-from-rat?)


(define (alternative-words s)
  (in-dictionary (words->strings (arrangements (string->word s)))))


; DICTIONARIES
(define DICTIONARY-LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-Strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

;List-of-strings -> List-of-strings
; pick out all those Strings that occur in the dictionary

(check-expect
 (in-dictionary
  (list "hi"
        "helw"
        "hello"))
 (list "hi" "hello"))

(check-expect
 (in-dictionary
  '())
 '())

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else
     (if
      (string-in-dict? (first los) DICTIONARY-AS-LIST)
      (cons (first los) (in-dictionary (rest los)))
      (in-dictionary (rest los)))]))

; String Dict -> Boolean
; is s in d?

(check-expect
 (string-in-dict?
  "hi" (list "hi" "hello"))
 #true)

(check-expect
 (string-in-dict?
  "hi" (list "hu" "hello"))
 #false)

(define (string-in-dict? s d)
  (cond [(empty? d) #false]
        [else
         (if
          (string=? s (first d))
          #true
          (string-in-dict? s (rest d)))]))

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of single Strings (letters)

(define w1 (list "h" "i"))
(define w2 (list "w" "h" "o"))


; A List-of-words is one of:
; - '() or
; - (cons word List-of-words)
; interpretation a list of words

(define l1 (list w1 w2))

; Word -> List-of-words
; find all re-arrangements of word

(check-expect
 (arrangements
  (list "w" "h"))
 (list 
  (list "w" "h")
  (list "h" "w")))

(check-expect
 (arrangements w1)
 (list
  (list "h" "i")
  (list "i" "h")))

(check-expect
 (arrangements '())
 (list '()))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else
     (insert-everywhere/in-all-words (first w)
                                     (arrangements (rest w)))]))


; 1String List-of-words -> List-of-words
; insert character into every position of all words in alow

(check-expect
 (insert-everywhere/in-all-words
  "d"
  (cons (list "e" "r")
        (cons (list "r" "e") '())))
 (list
  (list "d" "e" "r")
  (list "e" "d" "r")
  (list "e" "r" "d")
  (list "d" "r" "e")
  (list "r" "d" "e")
  (list "r" "e" "d")))

(check-expect
 (insert-everywhere/in-all-words
  "d" '())
 '())

(check-expect
 (insert-everywhere/in-all-words
  "d" (list (list "h")))
 (list
  (list "d" "h")
  (list "h" "d")))

(define (insert-everywhere/in-all-words s alow)
  (cond
    [(empty? alow) '()]
    [else
     (append
      (insert-everywhere s (first alow))
      (insert-everywhere/in-all-words s (rest alow)))]))

; 1String Word -> List-of-words
; insert s into every position of aw

(check-expect
 (insert-everywhere "e" (list "s" "h" "i"))
 (list
  (list "e" "s" "h" "i")
  (list "s" "e" "h" "i")
  (list "s" "h" "e" "i")
  (list "s" "h" "i" "e")))

(check-expect
 (insert-everywhere "s" (list "h" "i"))
 (list
  (list "s" "h" "i")
  (list "h" "s" "i")
  (list "h" "i" "s")))

(check-expect
 (insert-everywhere "s" (list "i"))
 (list
  (list "s" "i")
  (list "i" "s")))

(check-expect
 (insert-everywhere "s" '())
 (list (list "s")))

(define (insert-everywhere s aw)
  (cond
    [(empty? aw) (list (list s))]
    [else
     (cons
      (cons s aw)
      (append-all (first aw)
                  (insert-everywhere s (rest aw))))]
    ))

; 1String List-of-words -> List-of-words
; Add s to the start of every word in alow

(check-expect
 (append-all "s" (list (list "a" "b") (list "c" "d")))
 (list
  (list "s" "a" "b")
  (list "s" "c" "d")))

(check-expect
 (append-all "s" (list (list "w")))
 (list (list "s" "w")))

(check-expect
 (append-all "s" '())
 '())

(define (append-all s alow)
  (cond
    [(empty? alow) '()]
    [else
     (cons
      (cons s (first alow))
      (append-all s (rest alow)))]
    ))

; String -> Word
; convert s to the chosen word representation

(check-expect
 (string->word "hi")
 (cons "h" (cons "i" '())))

(check-expect
 (string->word "")
 '())

(define (string->word s)
  (cond
    [(string=? "" s) '()]
    [else
     (cons
      (substring s 0 1)
      (string->word (substring s 1 (string-length s))))]))

; Word -> String
; convert w to a string

(check-expect
 (word->string (list "h" "i"))
 "hi")

(check-expect
 (word->string '())
 "")


(define (word->string w)
  (cond
    [(empty? w) ""]
    [else
     (string-append
      (first w)
      (word->string (rest w)))]))

; List-of-words -> List-of-strings
; Turn all words in alow into strings

(check-expect
 (words->strings
  (list (list "h" "i") (list "w" "o" "w")))
 (list "hi" "wow"))

(check-expect
 (words->strings
  '())
 '())

(define (words->strings alow)
  (cond
    [(empty? alow) '()]
    [else
     (cons
      (word->string (first alow))
      (words->strings (rest alow)))]))

