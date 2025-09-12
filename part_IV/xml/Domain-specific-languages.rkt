;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Domain-specific-languages) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A FSM is a [List-of 1Transition]

; A 1Transition is a list of two items:
; (cons FSM-State (cons FSM-State '()))

; A FSM-State is a String that specifies color

; data examples
(define fsm-traffic
  '(("red" "green")("green" "yellow")("yellow" "red")))

; A FSM.v2 is a [List-of 1Transition.v2]

; A 1Transition.v2 is a list of two items:
; (cons (State-Key (cons FSM-State '()))

; A State-Key is a list of two items:
; (cons FSM-State (cons KeyEvent '()))

(define fsm-traffic.v2
  '((("red" "k") "green")
    (("green" "d") "yellow")
    (("yellow" "a") "red")))

; An XMachine is a nested list of this shape:
; (list 'machine (list (list 'initial FSM-State)) [List-of X1T])

; An X1T is a nested list of this shape:
; (list 'action (list
;                   (list 'state FSM-State)
;                   (list 'next FSM-State)))

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))
; <machine initial="white">
;   <action state="white" next="black" />
;   <action state="black" next="white" />
; </ machine>

(define xm1
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

; FUNCTIONS ======================================================

; FSM FSM-State -> FSM-State
; match the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  ; State of the World: FSM-State
  (big-bang state0
    [to-draw
     (lambda (current)
       (overlay (text current 14 "black")
                (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions current))]))

; [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in the association list

(check-expect (find '((red black) (green blue)) 'green) 'blue)
(check-error (find '((red black)) 'green) "next state not found")
(check-expect (find '((("red" "h") black)
                      (("green" "i") blue)) '("green" "i")) 'blue)

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "next state not found"))))

(define (find1 l x)
  (cond [(empty? l) (error "next state not found")]
        [else
         (if
          (equal? (first (first l)) x)
          (second (first l))
          (find (rest l) x))]))

; FSM.v2 FSM-State -> FSM-State
; match the keys pressed by a player with the given FSM.v2
(define (simulate.v2 state0 transitions)
  ; State of the World: FSM-State
  (big-bang state0
    [to-draw
     (lambda (current)
       (overlay (text current 14 "black")
                (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions (list current key-event)))]))

; XML PROCESSORS ==============================================

(define a0 '((initial "red")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Xexpr.v2]
; get the content of xe

(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content e2) '((action)))

(define (xexpr-content xe)
  (if
   (list-of-attributes? (second xe))
   (rest (rest xe))
   (rest xe)))

; Loa/xe -> Boolean
; deteremine whether x is an element of [List-of Attribute]
; #false otherwise
(check-expect (list-of-attributes? a0) #t)
(check-expect (list-of-attributes? e0) #f)
(check-expect (list-of-attributes? e4) #f)

(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; [List-of Attribute] Symbol -> [Maybe String]
; find attribute in loa using s as the key

(check-expect (lookup-attribute a0 'initial) "red")
(check-expect (lookup-attribute a0 'init) #f)

(define (lookup-attribute loa s)
  (local ((define pair (assq s loa)))
    (if (cons? pair)
        (second pair)
        #f
        )))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attributes e0) '())
(check-expect (xexpr-attributes e1) '((initial "red")))
(check-expect (xexpr-attributes e2) '())
(check-expect (xexpr-attributes e3) '())
(check-expect (xexpr-attributes e4) '((initial "red")))

(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))
; =============================================================

; XMachine -> FSM-State
; extract initial state from XMachine

(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm)
  (lookup-attribute (xexpr-attributes xm) 'initial))

; XMachine -> FSM
; translates the embedded [List-of X1T] into [List-of 1Transition]

(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (lookup-attribute
                   (xexpr-attributes xa) 'state)
                  (lookup-attribute
                   (xexpr-attributes xa) 'next))))
    (map xaction->action (xexpr-content xm))))


; XMachine -> FSM-State
; simulates an FSM via the given configuration
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

(simulate-xmachine xm1)

















