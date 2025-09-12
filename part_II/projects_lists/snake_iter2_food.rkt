;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake_iter2_food) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
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

; A Snake is one of:
; - '()
; - (cons Segment Snake)
; interpretation a list of segments represents a snake

; RENDER
; Snake -> Image
; Render a snake based on coordinates

(check-expect
 (render (list (make-segment 4 5 "d")))
 (place-image SEG
              (* 4 DIAM) (* 5 DIAM)
              BG))

(check-expect
 (render (list (make-segment 4 5 "d")
               (make-segment 7 19 "u")))
 (place-image SEG
              (* 4 DIAM) (* 5 DIAM)
              (place-image SEG
                           (* DIAM 7) (* 19 DIAM) BG)))

(check-expect
 (render '()) BG)
 

(define (render alos)
  (cond
    [(empty? alos) BG]
    [else
     (place-image SEG
                  (* (segment-from-left (first alos)) DIAM)
                  (* (segment-from-top (first alos)) DIAM)
                  (render (rest alos)))]))

; TOCK
; Snake -> Snake
; Move snake by one segment

(check-expect
 (tock
  (list
   (make-segment 6 6 "d")
   (make-segment 6 5 "d")
   (make-segment 6 4 "d")))
 (list
  (make-segment 6 7 "d")
  (make-segment 6 6 "d")
  (make-segment 6 5 "d")))

(check-expect
 (tock
  (list
   (make-segment 6 6 "d")
   (make-segment 6 5 "d")
   (make-segment 5 5 "r")))
 (list
  (make-segment 6 7 "d")
  (make-segment 6 6 "d")
  (make-segment 6 5 "d")))

(check-expect
 (tock
  (list
   (make-segment 6 6 "d")
   (make-segment 6 5 "d")
   (make-segment 5 5 "r")
   (make-segment 5 4 "d")))
 (list
  (make-segment 6 7 "d")
  (make-segment 6 6 "d")
  (make-segment 6 5 "d")
  (make-segment 5 5 "r")))

(check-expect
 (tock '())
 '())

(define (tock sn)
  (cond
    [(empty? sn) '()]
    [else (cons
           (move-snake
            (first sn)
            (segment-direction (first sn)))
           (remove-last sn))]))

; REMOVE-LAST
; Snake -> Snake
; remove last segment of snake

(check-expect
 (remove-last
  (list
   (list 3 4 "d")
   (list 3 5 "d")))
 (list (list 3 4 "d")))

(define (remove-last sn)
  (cond
    [(empty? (rest sn)) '()]
    [else (cons (first sn) (remove-last (rest sn)))]))

; MOVE-SNAKE
; Segment String -> Segment
; move s by one in the d direction

(check-expect
 (move-snake (make-segment 3 5 "d") "u")
 (make-segment 3 4 "u"))

(check-expect
 (move-snake (make-segment 3 5 "d") "r")
 (make-segment 4 5 "r"))

(check-expect
 (move-snake (make-segment 3 5 "d") "l")
 (make-segment 2 5 "l"))

(check-expect
 (move-snake (make-segment 3 5 "l") "d")
 (make-segment 3 6 "d"))

(define (move-snake s d)
  (cond
    [(string=? "r" d)
     (make-segment
      (add1 (segment-from-left s))
      (segment-from-top s)
      d)
     ]
    [(string=? "l" d)
     (make-segment
      (sub1 (segment-from-left s))
      (segment-from-top s)
      d)
     ]
    [(string=? "u" d)
     (make-segment
      (segment-from-left s)
      (sub1 (segment-from-top s))
      d)
     ]
    [(string=? "d" d)
     (make-segment
      (segment-from-left s)
      (add1 (segment-from-top s))
      d)
     ]))




; KEY
; Snake KeyEvent -> Snake
; Change direction of s

(check-expect
 (key (list (make-segment 4 5 "d")) "up")
 (list (make-segment 4 5 "u")))

(check-expect
 (key (list (make-segment 4 5 "d")) "left")
 (list (make-segment 4 5 "l")))

(check-expect
 (key (list (make-segment 4 5 "d")) "right")
 (list (make-segment 4 5 "r")))

(check-expect
 (key (list (make-segment 4 5 "r")) "down")
 (list (make-segment 4 5 "d")))

(check-expect
 (key (list (make-segment 4 5 "r")) "else")
 (list (make-segment 4 5 "r")))

(define (key sn ke)
  (cond
    [(empty? sn) '()]
    [(string=? "up" ke)
     (cons
      (change-segment-direction (first sn) "u")
      (rest sn))]
    [(string=? "down" ke)
     (cons
      (change-segment-direction (first sn) "d")
      (rest sn))]
    [(string=? "left" ke)
     (cons
      (change-segment-direction (first sn) "l")
      (rest sn))]
    [(string=? "right" ke)
     (cons
      (change-segment-direction (first sn) "r")
      (rest sn))]
    [else sn]))

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
 (end? (list (make-segment 3 4 "d"))) ; regular
 #false)

(check-expect
 (end? (list (make-segment 3 4 "d")
             (make-segment 3 5 "r"))) ; regular
 #false)

(check-expect
 (end? (list (make-segment 0 4 "d")
             (make-segment 3 5 "r"))) ; left bound
 #true)

(check-expect
 (end? (list (make-segment (/ WIDTH DIAM) 4 "d")
             (make-segment 3 5 "r"))) ;right bound
 #true)

(check-expect
 (end? (list (make-segment 3 (/ HEIGHT DIAM) "d")
             (make-segment 3 5 "r"))) ;bottom bound
 #true)

(check-expect
 (end? (list (make-segment 4 0 "d")
             (make-segment 3 5 "r"))) ;top bound
 #true)

(check-expect
 (end? (list
        (make-segment 3 3 "l")
        (make-segment 3 2 "d")
        (make-segment 2 2 "r")
        (make-segment 2 3 "u")
        (make-segment 2 4 "u")))
 #true)

(define (end? sn)
  (cond
    [(empty? (rest sn))
     (not (and
           (< 0 (segment-from-left (first sn)) (/ WIDTH DIAM))
           (< 0 (segment-from-top (first sn)) (/ HEIGHT DIAM))))]
    [else (or
           (bite-itself sn)
           (not (and
            (< 0 (segment-from-left (first sn)) (/ WIDTH DIAM))
            (< 0 (segment-from-top (first sn)) (/ HEIGHT DIAM)))))]))

; BITE-ITSELF
; Snake -> Boolean

(check-expect
 (bite-itself
  (list
   (make-segment 3 5 "u")
   (make-segment 3 6 "u")))
 #false)

(check-expect
 (bite-itself
  (list
   (make-segment 3 3 "l")
   (make-segment 3 2 "d")
   (make-segment 2 2 "r")
   (make-segment 2 3 "u")
   (make-segment 2 4 "u")))
 #true)

(define (bite-itself sn)
  (member-pos?
   (move-snake
    (first sn)
    (segment-direction (first sn)))
   (remove-last (rest sn))))

; MEMBER-POS?
; check whether s is in sn, not considering the direction
; Segment Snake -> Boolean

(check-expect
 (member-pos?
  (make-segment 3 5 "d")
  (list
   (make-segment 3 5 "d")
   (make-segment 3 6 "d")))
 #true)

(check-expect
 (member-pos?
  (make-segment 3 5 "d")
  (list
   (make-segment 3 4 "d")
   (make-segment 3 6 "d")))
 #false)

(check-expect
 (member-pos?
  (make-segment 3 5 "d") '())
 #false)

(define (member-pos? s sn)
  (cond
    [(empty? sn) #false]
    [else
     (if
      (segment-same-pos? s (first sn))
      #true
      (member-pos? s (rest sn)))]))

; SEGMENT-SAME-POS?
; Segment Segment -> Boolean

(check-expect
 (segment-same-pos? (make-segment 3 5 "d") (make-segment 3 5 "r"))
 #true)

(check-expect
 (segment-same-pos? (make-segment 3 4 "d") (make-segment 3 5 "r"))
 #false)

(define (segment-same-pos? s1 s2)
  (and
   (= (segment-from-left s1) (segment-from-left s2))
   (= (segment-from-top s1) (segment-from-top s2))))

; END SCREEN
(define (end s)
  (overlay/align
   "left" "bottom"
   (text "Snake Out of Bounds or Bit Itself" 10 "black")
   BG))


; Posn -> Posn
; randomly place food
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (food-check-create p (make-posn (random MAX) (random MAX))))

; Posn Posn -> Posn
; generative recursion
; check that the new food isn't placed on posn p
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; testing function to see if the posn is equal to (1, 1)
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))




; MAIN
; Snake -> Snake
; Starts program from a given initial state
(define (main s)
  (big-bang s
    [on-draw render]
    [on-tick tock 1]
    [on-key key]
    [stop-when end? end]))
(main (list (make-segment 5 4 "d")))

(define-struct ws [snake food])
; WorldState is a structure:
; (make-ws Snake Posn)
; interpretation (make-ws s p)
; s is the snake, p is the posn of the food

; WorldState -> Image
; Render the world using ws
(define (render-sf ws)
  (empty-scene 10 10))

; WorldState -> WorldState
; Change the state every second
(define (tock-sf ws)
  ws)

; MAIN-FOOD





















