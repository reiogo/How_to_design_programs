;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake-abstraction) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Abstraction - Snake

; Snake
; logical positioning

; constraints
(define UNIT 7)
(define DIAM (* 0.6 UNIT))
(define HEIGHT (* 40 UNIT))
(define WIDTH (* 40 UNIT))
(define BG
  (empty-scene HEIGHT WIDTH))

(define SEG
  (circle DIAM "solid" "red"))

(define FOOD
  (circle DIAM "solid" "green"))
; A [List-of [Maybe String]] is one of:
; - '()
; - (cons [Maybe String] [List-of [Maybe String])

(define-struct segment [from-left from-top direction])
; segment is a structure:
; (make-segment Number Number Direction)
; interpretation (make-segment fl ft d) fl is number of segments
; from the left, ft is the number of segments from the top
; d is the direction the segment will move in

; Direction is one of:
; - "left" for left
; - "right" for right
; - "up" for up
; - "down" for down

(define eg1 (make-segment 4 5 "up"))

; A Snake is one of:
; - '()
; - (cons Segment Snake)
; interpretation a list of segments represents a snake

; RENDER
; Snake -> Image
; Render a snake based on coordinates

(check-expect
 (render (list (make-segment 4 5 "down")))
 (place-image SEG
              (* 4 UNIT) (* 5 UNIT)
              BG))

(check-expect
 (render (list (make-segment 4 5 "down")
               (make-segment 7 19 "up")))
 (place-image SEG
              (* 4 UNIT) (* 5 UNIT)
              (place-image SEG
                           (* UNIT 7) (* 19 UNIT) BG)))

(check-expect
 (render '()) BG)
 

(define (render alos)
  (local (; Segment Image -> Image
          ; place segment on image
          (define (place-segment s0 sc)
            (place-image SEG
                         (* (segment-from-left s0) UNIT)
                         (* (segment-from-top s0) UNIT)
                         sc)))
    (foldr place-segment BG alos)))


; TOCK
; Snake -> Snake
; Move snake by one segment

(check-expect
 (tock
  (list
   (make-segment 6 6 "down")
   (make-segment 6 5 "down")
   (make-segment 6 4 "down")))
 (list
  (make-segment 6 7 "down")
  (make-segment 6 6 "down")
  (make-segment 6 5 "down")))

(check-expect
 (tock
  (list
   (make-segment 6 6 "down")
   (make-segment 6 5 "down")
   (make-segment 5 5 "right")))
 (list
  (make-segment 6 7 "down")
  (make-segment 6 6 "down")
  (make-segment 6 5 "down")))

(check-expect
 (tock
  (list
   (make-segment 6 6 "down")
   (make-segment 6 5 "down")
   (make-segment 5 5 "right")
   (make-segment 5 4 "down")))
 (list
  (make-segment 6 7 "down")
  (make-segment 6 6 "down")
  (make-segment 6 5 "down")
  (make-segment 5 5 "right")))

(check-expect
 (tock '())
 '())

(define (tock sn)
  (cond
    [(empty? sn) '()]
    [else (cons
           (move-segment
            (first sn)
            (segment-direction (first sn)))
           (remove-last sn))]))

; REMOVE-LAST
; Snake -> Snake
; remove last segment of snake

(check-expect
 (remove-last
  (list
   (list 3 4 "down")
   (list 3 5 "down")))
 (list (list 3 4 "down")))

(define (remove-last sn)
  (cond
    [(empty? (rest sn)) '()]
    [else (cons (first sn) (remove-last (rest sn)))]))

; MOVE-SEGMENT
; Segment String -> Segment
; move s by one in the d direction

(check-expect
 (move-segment (make-segment 3 5 "down") "up")
 (make-segment 3 4 "up"))

(check-expect
 (move-segment (make-segment 3 5 "down") "right")
 (make-segment 4 5 "right"))

(check-expect
 (move-segment (make-segment 3 5 "down") "left")
 (make-segment 2 5 "left"))

(check-expect
 (move-segment (make-segment 3 5 "left") "down")
 (make-segment 3 6 "down"))

(define (move-segment s d)
  (cond
    [(string=? "right" d)
     (make-segment
      (add1 (segment-from-left s))
      (segment-from-top s)
      d)
     ]
    [(string=? "left" d)
     (make-segment
      (sub1 (segment-from-left s))
      (segment-from-top s)
      d)
     ]
    [(string=? "up" d)
     (make-segment
      (segment-from-left s)
      (sub1 (segment-from-top s))
      d)
     ]
    [(string=? "down" d)
     (make-segment
      (segment-from-left s)
      (add1 (segment-from-top s))
      d)
     ]))

; KEY
; Snake KeyEvent -> Snake
; Change direction of s

(check-expect
 (key (list (make-segment 4 5 "down")) "up")
 (list (make-segment 4 5 "up")))

(check-expect
 (key (list (make-segment 4 5 "down")) "left")
 (list (make-segment 4 5 "left")))

(check-expect
 (key (list (make-segment 4 5 "down")) "right")
 (list (make-segment 4 5 "right")))

(check-expect
 (key (list (make-segment 4 5 "right")) "down")
 (list (make-segment 4 5 "down")))

(check-expect
 (key (list (make-segment 4 5 "right")) "else")
 (list (make-segment 4 5 "right")))

(check-expect
 (key (list (make-segment 4 5 "up")
            (make-segment 4 6 "up")) "down")
 (list (make-segment 4 5 "up")
       (make-segment 4 6 "up")))

(check-expect
 (key (list (make-segment 4 5 "up")
            (make-segment 4 6 "up")) "right")
 (list (make-segment 4 5 "right")
       (make-segment 4 6 "up")))

(check-expect
 (key (list (make-segment 4 5 "up")
            (make-segment 4 6 "up")) "left")
 (list (make-segment 4 5 "left")
       (make-segment 4 6 "up")))

(check-expect
 (key (list (make-segment 4 5 "up")
            (make-segment 4 6 "up")
            (make-segment 4 7 "up")) "left")
 (list (make-segment 4 5 "left")
       (make-segment 4 6 "up")
       (make-segment 4 7 "up")))

(check-expect
 (key (list (make-segment 4 5 "up")
            (make-segment 4 6 "up")
            (make-segment 4 7 "up")) "down")
 (list (make-segment 4 5 "up")
       (make-segment 4 6 "up")
       (make-segment 4 7 "up")))

(define (key sn ke)
  (local ((define DIRECTIONS '("right" "left" "down" "up"))
          ; Segment Segment -> Boolean
          ; If snake goes back will it hit itself?
          (define (not-back? s0 s1)
            (not (and
             (= (segment-from-top (move-segment s0 ke)) (segment-from-top s1))
             (=  (segment-from-left (move-segment s0 ke)) (segment-from-left s1)))
            )))
    (cond
      [(empty? sn) '()]
      [(and
        (member? ke DIRECTIONS)
        (empty? (rest sn)))
       (cons
        (change-segment-direction (first sn) ke)
        (rest sn))]
      [(and
        (member? ke DIRECTIONS)
       (not-back? (first sn) (second sn)))
       (cons
        (change-segment-direction (first sn) ke)
        (rest sn))]
      [else sn])))

; CHANGE-SNAKE-DIRECTION
; Snake String -> Snake
; Change the direction of the snake

(check-expect
 (change-segment-direction (make-segment 4 5 "up") "down")
 (make-segment 4 5 "down"))
 
(define (change-segment-direction s d)
  (make-segment
   (segment-from-left s)
   (segment-from-top s)
   d))

; END?
; Snake -> Boolean
; Check if program has ended

(check-expect
 (end? (list (make-segment 3 4 "down"))) ; regular
 #false)

(check-expect
 (end? (list (make-segment 3 4 "down")
             (make-segment 3 5 "right"))) ; regular
 #false)

(check-expect
 (end? (list (make-segment 0 4 "down")
             (make-segment 3 5 "right"))) ; left bound
 #true)

(check-expect
 (end? (list (make-segment (/ WIDTH UNIT) 4 "down")
             (make-segment 3 5 "right"))) ;right bound
 #true)

(check-expect
 (end? (list (make-segment 3 (/ HEIGHT UNIT) "down")
             (make-segment 3 5 "right"))) ;bottom bound
 #true)

(check-expect
 (end? (list (make-segment 4 0 "down")
             (make-segment 3 5 "right"))) ;top bound
 #true)

(check-expect
 (end? (list
        (make-segment 3 3 "left")
        (make-segment 3 2 "down")
        (make-segment 2 2 "right")
        (make-segment 2 3 "up")
        (make-segment 2 4 "up")))
 #true)

(define (end? sn)
  (cond
    [(empty? (rest sn))
     (not (and
           (< 0 (segment-from-left (first sn)) (/ WIDTH UNIT))
           (< 0 (segment-from-top (first sn)) (/ HEIGHT UNIT))))]
    [else (or
           (bite-itself sn)
           (not (and
                 (< 0 (segment-from-left (first sn)) (/ WIDTH UNIT))
                 (< 0 (segment-from-top (first sn)) (/ HEIGHT UNIT)))))]))

; BITE-ITSELF
; Snake -> Boolean

(check-expect
 (bite-itself
  (list
   (make-segment 3 5 "up")
   (make-segment 3 6 "up")))
 #false)

(check-expect
 (bite-itself
  (list
   (make-segment 3 3 "left")
   (make-segment 3 2 "down")
   (make-segment 2 2 "right")
   (make-segment 2 3 "up")
   (make-segment 2 4 "up")))
 #true)

(define (bite-itself sn)
  (member-pos?
   (move-segment
    (first sn)
    (segment-direction (first sn)))
   (remove-last (rest sn))))

; MEMBER-POS?
; check whether s is in sn, not considering the direction
; Segment Snake -> Boolean

(check-expect
 (member-pos?
  (make-segment 3 5 "down")
  (list
   (make-segment 3 5 "down")
   (make-segment 3 6 "down")))
 #true)

(check-expect
 (member-pos?
  (make-segment 3 5 "down")
  (list
   (make-segment 3 4 "down")
   (make-segment 3 6 "down")))
 #false)

(check-expect
 (member-pos?
  (make-segment 3 5 "down") '())
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
 (segment-same-pos? (make-segment 3 5 "down") (make-segment 3 5 "right"))
 #true)

(check-expect
 (segment-same-pos? (make-segment 3 4 "down") (make-segment 3 5 "right"))
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


; FOOD-CREATE
; Posn -> Posn
; randomly place food
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (food-check-create p
                     (make-posn
                      (add1 (random (sub1 (/ WIDTH UNIT))))
                      (add1 (random (sub1 (/ HEIGHT UNIT)))))))

; FOOD-CHECK-CREATE
; Posn Posn -> Posn
; generative recursion
; check that the new food isn't placed on posn p
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; NOT-EQUAL-1-1
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
; (main (list (make-segment 5 4 "down")))

(define-struct ws [snake food])
; WorldState is a structure:
; (make-ws Snake Posn)
; interpretation (make-ws s p)
; s is the snake, p is the posn of the food

;RENDER-SF
; WorldState -> Image
; Render the world using ws

(check-expect
 (render-sf (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)))
 (place-image FOOD
              (* 3 UNIT) (* 5 UNIT)
              (render (list (make-segment 3 2 "down")))))

(define (render-sf ws)
  (place-image FOOD
               (* (posn-x (ws-food ws)) UNIT)
               (* (posn-y (ws-food ws)) UNIT)
               (render (ws-snake ws))))

; TOCK-SF 
; WorldState -> WorldState
; Change the state every second

(check-expect
 (tock-sf (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)))
 (make-ws (list (make-segment 3 3 "down")) (make-posn 3 5)))

(define (tock-sf w)
  (if
   (eat? (first (ws-snake w)) (ws-food w))
   (make-ws
    (grow w)
    (food-create (ws-food w)))
   (make-ws
    (tock (ws-snake w))
    (ws-food w))))

; GROW
; WorldState -> Snake

(check-expect
 (grow (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)))
 (list
  (make-segment 3 5 "down")
  (make-segment 3 2 "down")))

(define (grow w)
  (cons
   (make-segment
    (posn-x (ws-food w))
    (posn-y (ws-food w))
    (segment-direction (first (ws-snake w))))
   (ws-snake w)))

; EAT?
; Segment Posn -> Boolean
; Check if the snake head is about to eat the food or not

(check-expect
 (eat? (make-segment 3 5 "up") (make-posn 3 4))
 #true)

(check-expect
 (eat? (make-segment 3 5 "up") (make-posn 3 6))
 #false)


(define (eat? s p)
  (and
   (= (segment-from-left (move-segment s (segment-direction s)))
      (posn-x p))
   (= (segment-from-top (move-segment s (segment-direction s)))
      (posn-y p))
   ))

;KEY-SF
; WorldState KeyEvent -> WorldState

(check-expect
 (key-sf (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)) "down")
 (make-ws
  (key (list (make-segment 3 2 "down")) "down")
  (make-posn 3 5)))

(define (key-sf w ke)
  (make-ws
   (key (ws-snake w) ke)
   (ws-food w)))

; END-SF?
; WorldState -> Boolean

(check-expect
 (end-sf? (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)))
 (end? (list (make-segment 3 2 "down"))))

(define (end-sf? w)
  (end? (ws-snake w)))

; MAIN-FOOD
(define (main-food ws)
  (big-bang ws
    [on-draw render-sf]
    [on-tick tock-sf 0.15]
    [on-key key-sf]
    [stop-when end-sf? end]))
(main-food (make-ws (list (make-segment 3 2 "down")) (make-posn 3 5)))

