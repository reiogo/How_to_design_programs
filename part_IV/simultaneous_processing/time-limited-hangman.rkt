;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname time-limited-hangman) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; time-limited hangman
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
(define DICTIONARY-LOCATION "/usr/share/dict/words") ; on Mac OS X
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define DICTIONARY-SIZE (length DICTIONARY-AS-LIST))

; A HM-Word is a [List-of [Maybe Letter]]
; interpretation #false represents a letter to be guessed

; HM-Word N -> String
; run a simplistic Hangman game, produce the current state of
; the game
; assume the-pick does not contain #false
(define (play the-pick time-limit)
  (local ((define the-word (explode the-pick))
          (define the-guess (make-list (length the-word) #false))

          ; HM-Word -> HM-Word
          (define (do-nothing s) s)

          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))

    ; the state of the game is a HM-Word

    (implode
     (big-bang the-guess
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key checked-compare]))))

; HM-Word -> Image
; render the word, using "_" for places that are #false
(define (render-word w)
  (local ((define l (map (lambda (lt) (if (boolean? lt) "_" lt)) w))
          (define s (implode l)))
    (text s 22 "black")))




; HM-Word KeyEvent -> HM-Word
; compares the answer to the player's word so far
; result is the new hm-word if the guess was good,
; the old hm-word if the guess was a miss.

(check-expect
 (compare-word '() '() "i")
 '())
(check-expect
 (compare-word '("h" "i" "v" "e") '("h" #false #false "e") "i")
 '("h" "i" #false "e"))
(check-expect
 (compare-word '("h" "i" "v" "e") '("h" #false #false "e") "r")
 '("h" #false #false "e"))
(check-expect
 (compare-word '("h" "i" "g" "h") '( #false "i" "g" #false) "h")
 '("h" "i" "g" "h"))

(define (compare-word answer current guess)
  (cond
    [(empty? answer) '()]
    [else
     (local ((define answ-letter (first answer))
             (define curr-letter (first current)))
               (if
      (and (string=? guess answ-letter)
           (boolean? curr-letter))
      (cons guess
            (compare-word (rest answer) (rest current) guess))
      (cons curr-letter
            (compare-word (rest answer) (rest current) guess))))
            ]))


;(play (list-ref DICTIONARY-AS-LIST (random DICTIONARY-SIZE)) 10)













       