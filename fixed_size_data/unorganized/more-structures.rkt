;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname more-structures) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct r3 [ x y z])
; A R3 is a structure:
;(make-r3 Number Number Number)

(define eg1 (make-r3 1 2 13))
(define eg2 (make-r3 -1 0 3))



; R3 -> Number
; computes the distance of p to an origin in 3d space
(check-within (r3-distance-to-0 eg1) (sqrt 174) 0.001)
(check-within (r3-distance-to-0 eg2) (sqrt 10) 0.001)
(define (r3-distance-to-0 p)
  (sqrt (+ (sqr (r3-x p)) (sqr (r3-y p)) (sqr (r3-z p)))))

(define-struct movie [title director year])
(define (process-movie m)
  (...(movie-title m)... (movie-director m) ...(movie-year)))

(define-struct person [name hair eyes phone])
(define (person-add p)
  (... (person-name p) ... (person-hair p) ... (person-eyes p)
       ... (person-phone p)))

(define-struct pet [name number])
(define (create-pet p)
  (... (pet-name p) ... (pet-number p)))

(define-struct CD [artist title price])
(define (register-CD c)
  (... (CD-artist c) ... (CD-title c) ... (CD-price c)))

(define-struct sweater [material size color])
(define (sweater-sell s)
  (... (sweater-material s) ... (sweater-size s)
       ... (sweater-color s)))

(define-struct time-from-midnight [hours minutes seconds])
; A Time-from-midnight is a structure
; (make-time-from-midnight Number Number Number)
; interpretation, (make-time-from-midnight h m s) represents a point in time from midnight where:
; h a number that describes how many hours its been since midnight (0 - 23)
; m a number that describes how many minutes it is past the hour (0 - 59).
; s a number that describes how many seconds it is past the minute (0 - 59).

; Time-from-midnight -> Number
; Convert time from midnight into seconds
(check-expect (time->seconds (make-time-from-midnight 12 30 2))
              45002)
(check-expect (time->seconds (make-time-from-midnight 0 0 0))
              0)
(check-expect (time->seconds (make-time-from-midnight 23 59 59))
              86399)
(define (time->seconds t)
  (+ (* 60  60 (time-from-midnight-hours t))
     (* 60 (time-from-midnight-minutes t))
     (time-from-midnight-seconds t)))

(define-struct 3lw [one two three])
; A Lc-3-letter-w is a structure:
; (make-lc-3-letter-w letterOrFalse letterOrFalse letterOrFalse)
; interpretation: represents a three letter word where each position
; [one, two, three] can be a single lowercase letter or a #false.
; #false indicates that the letter in the location is unknown or yet to be guessed.

; A letterOrFalse is one of:
; -String (a single lowercase letter from a-z)
; -Boolean (#false)


;3lw 3lw -> 3lw
; Compare two representations of three letter words
(check-expect (comp-3lw
               (make-3lw "t" "e" "a")
               (make-3lw "t" "e" "a"))
              (make-3lw "t" "e" "a"))
(check-expect (comp-3lw
               (make-3lw "t" "e" "a")
               (make-3lw "t" "h" "e"))
              (make-3lw "t" #false #false))
(define (comp-3lw w1 w2)
  (make-3lw (comp-l (3lw-one w1) (3lw-one w2))
  (comp-l (3lw-two w1) (3lw-two w2))
(comp-l (3lw-three w1) (3lw-three w2))))

;String String -> letterOrFalse
; compare letters and return letterOrFalse
(check-expect (comp-l "t" "t") "t")
(check-expect (comp-l "f" "e") #false)
(define (comp-l l1 l2)
  (cond
    [(string=? l1 l2) l2]
    [else #false]))










