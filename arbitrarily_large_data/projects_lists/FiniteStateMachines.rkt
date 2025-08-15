;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname FiniteStateMachines) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
; Finite State Machines

; A FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
; (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to key strokes

; example:
(define fsm-traffic
  (list
   (make-transition "red" "green")
   (make-transition "green" "yellow")
   (make-transition "yellow" "red")))

; BW Machine
(define bw-machine
  (list
   (make-transition "black" "white")
   (make-transition "white" "black")))

; A SimulationState.v1 is a FSM-State.

(define-struct fs [fsm current])
; A SimulationState.v2 is a strcuture:
; (make-fs FSM FSM-State)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
; (make-ktransitions FSM-State KeyEvent FSM-State)

; abcd
(define abcd
  (list
   (make-ktransition "white" "a" "yellow")
   (make-ktransition "yellow" "b" "yellow")
   (make-ktransition "yellow" "c" "yellow")
   (make-ktransition "yellow" "d" "green")))

(define-struct fsm [initial transitions final])
;(define-struct transition [current key next])
; An FSM.v3 is a structure:
; (make-fsm FSM-State LOT FSM-State)
; A LOT is one of:
; - '()
; - (cons Transition.v2 LOT)
; A Transition.v3 is a structure:
; (make-transition FSM-State FSM-State KeyEvent)

(define abcd.v2
  (make-fsm
   "white" 
   (list
    (make-ktransition "white" "a" "yellow")
    (make-ktransition "yellow" "b" "yellow")
    (make-ktransition "yellow" "c" "yellow"))
   "green"
   ))

   
; FIND-NEXT-STATE.V3 ================================================

; FSM.v2 KeyEvent-> FSM.v2
; finds the next state from a key stroke ke and current state cs

(check-expect
 (find-next-state.v3 abcd.v2 "a")
 (make-fsm
 "yellow"
  (list
   (make-ktransition "white" "a" "yellow")
   (make-ktransition "yellow" "b" "yellow")
   (make-ktransition "yellow" "c" "yellow"))
  "green"
  ))
 

(define (find-next-state.v3 a-fs ke)
  (make-fsm
   (find.v2
    (fsm-transitions a-fs)
     (fsm-initial a-fs)
    ke)
   (fsm-transitions a-fs)
   (fsm-final a-fs)))


; STATE-AS-COLORED-SQUARE.V2 ================================================

; SimulationState.v2 -> Image
; renders current world state as a colored square

(check-expect
 (state-as-colored-square.v2 abcd.v2)
 (square 100 "solid" "white"))

(define (state-as-colored-square.v2 a-fsm)
  (square 100 "solid" (fsm-initial a-fsm)))


; FSM-SIMULATE ================================================

; FSM.v2 -> FSM.v2
; match the keys pressed by a player with the given FSM
(define (fsm-simulate a-fsm s0 send)
  (big-bang (make-fsm s0 a-fsm send)
    [to-draw state-as-colored-square.v2]
    [on-key find-next-state.v3]
    ))

;STATE=? ================================================

; FSM-State FSM-State -> Boolean
; Check equality of fsm states

(check-expect
 (state=? "red" "green") #false)

(check-expect
 (state=? "red" "red") #true)


(define (state=? f0 f1)
  (string=? f0 f1))

; RENDER-STATE.V1 ================================================

; SimulationState.v1 -> Image
; renders a world state as an image
(define (render-state.v1 s)
  empty-image)

; FIND-NEXT-STATE.V1 ================================================

; SimulationState.v1 -> SimulationState.v1
; finds the next state from a key stroke ke and current state cs
(define (find-next-state.v1 cs ke)
  cs)

; SIMULATE.v1 ================================================


; FSM -> SimulationState.v1
; match the keys pressed by a player with the given FSM
(define (simulate.v1 fsm0)
  (big-bang fsm0
    ;initial-state
    [to-draw render-state.v1]
    [on-key find-next-state.v1]))

; RENDER-STATE.V2 ================================================

; SimulationState.v2 -> Image
; renders a world state as an image
(define (render-state.v2 s)
  empty-image)


; STATE-AS-COLORED-SQUARE ================================================

; SimulationState.v2 -> Image
; renders current world state as a colored square

(check-expect
 (state-as-colored-square (make-fs fsm-traffic "red"))
 (square 100 "solid" "red"))

(define (state-as-colored-square a-fs)
  (square 100 "solid" (fs-current a-fs)))


; FIND-NEXT-STATE.V2 ================================================

; SimulationState.v2 KeyEvent-> SimulationState.v2
; finds the next state from a key stroke ke and current state cs

(check-expect (find-next-state.v2 (make-fs fsm-traffic "red") "n")
              (make-fs fsm-traffic "green"))

(check-expect (find-next-state.v2 (make-fs fsm-traffic "red") "a")
              (make-fs fsm-traffic "green"))

(check-expect (find-next-state.v2 (make-fs fsm-traffic "green") "q")
              (make-fs fsm-traffic "yellow"))

(check-expect (find-next-state.v2 (make-fs fsm-traffic "yellow") "n")
              (make-fs fsm-traffic "red"))

(define (find-next-state.v2 a-fs ke)
  (make-fs
   (fs-fsm a-fs)
   (cond
     [(transition? (first (fs-fsm a-fs)))
      (find (fs-fsm a-fs) (fs-current a-fs))]
     [(ktransition? (first (fs-fsm a-fs)))
      (find.v2 (fs-fsm a-fs) (fs-current a-fs) ke)])
   ))

; FIND.V2 ================================================

; FSM FSM-State KeyEvent-> FSM-State
; finds the state matching current in the transition table

(check-expect (find.v2 abcd "white" "a") "yellow")

(check-expect (find.v2 abcd "yellow" "b") "yellow")

(check-expect (find.v2 abcd "yellow" "c") "yellow")

(check-expect (find.v2 abcd "yellow" "d") "green")

(check-expect (find.v2 abcd "white" "d") "red")

(check-expect (find.v2 abcd "yellow" "a") "red")

(define (find.v2 transitions current ke)
  (cond
    [(empty? transitions) "red"]
    [else
     (if
      (and
       (string=? (ktransition-current (first transitions)) current)
       (string=? (ktransition-key (first transitions)) ke))
      (ktransition-next (first transitions))
      (find.v2 (rest transitions) current ke))]))



; FIND ================================================

; FSM FSM-State -> FSM-State
; finds the state matching current in the transition table

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "purple") "not found: purple")
(check-error (find fsm-traffic "white") "not found: white")
(check-error (find fsm-traffic "black") "not found: black")
(check-error (find fsm-traffic "send") "not found")

(define (find transitions current)
  (cond
    [(empty? transitions)
     (cond
       [(string=? "purple" current) (error "not found: purple")]
       [(string=? "white" current) (error "not found: white")]
       [(string=? "black" current) (error "not found: black")]
       [else (error "not found")])]
    [else
     (if
      (string=? (transition-current (first transitions)) current)
      (transition-next (first transitions))
      (find (rest transitions) current))]))

; SIMULATE.v2 ================================================

; FSM -> SimulationState.v2
; match the keys pressed by a player with the given FSM
(define (simulate.v2 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state.v2]
    ))

; (simulate.v2 fsm-traffic "red")
; (simulate.v2 bw-machine "white")

; SIMULATE.v3 ================================================

; FSM -> SimulationState.v2
; match the keys pressed by a player with the given FSM
(define (simulate.v3 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state.v2]
    ))
; (simulate.v3 abcd "white")

; SIMULATE ================================================

; FSM -> ???
; match teh keys pressed by a player with the given fsm
(define (simulate a-fsm)
  (big-bang ...
    [to-draw ...]
    [on-key ...]))





























