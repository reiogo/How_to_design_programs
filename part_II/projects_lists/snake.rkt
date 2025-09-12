;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; Snake
; logical positioning

; constraints
(define UNIT 5)
(define DIAM (* 1 UNIT))
(define HEIGHT (* 40 UNIT))
(define WIDTH (* 40 UNIT))
(define BG
  (empty-scene HEIGHT WIDTH))

(define SEG
  (circle DIAM "solid" "red"))

(define FOOD
  (circle DIAM "solid" "green"))

(define-struct segment [from-left from-top direction])
; segment is a structure:
; (make-segment Number Number Direction)
; interpretation (make-segment fl ft d) fl is number of segments
; from the left, ft is the number of segments from the top
; d is the direction the segment will move in

; Direction is one of:
; - "l" for left
; - "r" for right
; - "u" for up
; - "d" for down

(define eg1 (make-segment 4 5 "u"))

(define-struct snake [head tail])
; snake is a structure:
; (make-snake Segment List-of-Segments)
; interpretation (make-snake h alos)
; h is a head segment and alos is the list of tail segments

; RENDER
; Snake -> Image
; Render a snake based on coordinates

(check-expect
 (render (make-snake (make-segment 5 3 "d") '()))
 (place-image SEG
              (* 5 DIAM) (* 3 DIAM)
              BG))

(define (render s)
  (place-image SEG
               (* (segment-from-left (snake-head s)) DIAM)
               (* (segment-from-top (snake-head s)) DIAM)
               (render-tail (snake-tail s))))

; RENDER-TAIL
; List-of-segments -> Image
; render the tail segments

(check-expect
 (render-tail (list (make-segment 4 5 "d")))
 (place-image SEG
              4 5
              BG))

(check-expect
 (render-tail (list (make-segment 4 5 "d")
                    (make-segment 7 19 "u")))
 (place-image SEG
              4 5
              (place-image SEG
                           7 19 BG)))

(check-expect
 (render-tail '()) BG)
 

(define (render-tail alos)
  (cond
    [(empty? alos) BG]
    [else
     (place-image SEG
      (segment-from-left (first alos))
      (segment-from-top (first alos))
      (render-tail (rest alos)))]))

; TOCK
; Snake -> Snake
; Move snake by one diameter in the direction given

(check-expect
 (tock (make-snake
        (make-segment 5 3 "d")
        '()))
 (make-segment 5 4 "d"))
(check-expect
 (tock (make-snake
        (make-segment 5 3 "d")
        (list
         (make-segment 6 3 "l")
         (make-segment 7 3 "l"))))
 (make-segment 5 4 "d"))

(check-expect
 (tock (make-segment 5 3 "r"))
 (make-segment 6 3 "r"))

(check-expect
 (tock (make-segment 5 3 "l"))
 (make-segment 4 3 "l"))

(check-expect
 (tock (make-segment 5 3 "u"))
 (make-segment 5 2 "u"))

(check-expect
 (tock (make-segment 5 3 "f"))
 (make-segment 5 3 "f"))

(define (tock s)
  (cond
    [(string=? "r" (segment-direction s))
     (make-segment
      (add1 (segment-from-left s))
      (segment-from-top s)
      (segment-direction s))
     ]
    [(string=? "l" (segment-direction s))
     (make-segment
      (sub1 (segment-from-left s))
      (segment-from-top s)
      (segment-direction s))
     ]
    [(string=? "u" (segment-direction s))
     (make-segment
      (segment-from-left s)
      (sub1 (segment-from-top s))
      (segment-direction s))
     ]
    [(string=? "d" (segment-direction s))
     (make-segment
      (segment-from-left s)
      (add1 (segment-from-top s))
      (segment-direction s))
     ]
    [else s]))

; KEY
; Snake KeyEvent -> Snake
; Change direction of s

(check-expect
 (key (make-segment 4 5 "d") "up")
 (make-segment 4 5 "u"))

(check-expect
 (key (make-segment 4 5 "d") "left")
 (make-segment 4 5 "l"))

(check-expect
 (key (make-segment 4 5 "d") "right")
 (make-segment 4 5 "r"))

(check-expect
 (key (make-segment 4 5 "r") "down")
 (make-segment 4 5 "d"))
(check-expect
 (key (make-segment 4 5 "r") "else")
 (make-segment 4 5 "r"))

(define (key s ke)
  (cond
    [(string=? "up" ke)
     (change-segment-direction s "u")]
    [(string=? "down" ke)
     (change-segment-direction s "d")]
    [(string=? "left" ke)
     (change-segment-direction s "l")]
    [(string=? "right" ke)
     (change-segment-direction s "r")]
    [else s]))

; CHANGE-SNAKE-DIRECTION
; Snake String -> Snake
; Change the direction of the snake

(check-expect
 (change-segment-direction (make-segment 4 5 "u") "d")
 (make-segment 4 5 "d"))
 
(define (change-segment-direction s d)
  (make-segment
   (segment-from-left s)
   (segment-from-top s)
   d))

; END?
; Snake -> Boolean
; Check if program has ended

(check-expect
 (end? (make-segment 3 4 "d")) ; regular
 #false)

(check-expect
 (end? (make-segment 0 4 "d")) ; left bound
 #true)

(check-expect
 (end? (make-segment (/ WIDTH DIAM) 4 "d")) ;right bound
 #true)

(check-expect
 (end? (make-segment 3 (/ HEIGHT DIAM) "d")) ;bottom bound
 #true)

(check-expect
 (end? (make-segment 4 0 "d")) ;top bound
 #true)

(define (end? s)
  (not (and
        (< 0 (segment-from-left s) (/ WIDTH DIAM))
        (< 0 (segment-from-top s) (/ HEIGHT DIAM)))))

; END SCREEN
(define (end s)
  (overlay/align
   "left" "bottom"
   (text "Snake Out of Bounds" 15 "black")
   BG))


; MAIN
; Snake -> Snake
; Starts program from a given initial state
(define (main s)
  (big-bang s
    [on-draw render]
    [on-tick tock 1]
    [on-key key]
    [stop-when end? end]))
;(main (make-segment 5 4 "d"))




















