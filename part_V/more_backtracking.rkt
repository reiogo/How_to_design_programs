;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname more_backtracking) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; More on Backtracking


; FSM =========================================================

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; A FSM is a structure:
; (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
; (make-transition FSM-State 1String FSM-State)
; A FSM-State is String.
; data example: see exercise 110

(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
; Does the sequence of chars in s cause the fsm to go
; from initial to final state?

; 4 questions for generative recursion
; 1. trivial case -> initial is final of the fsm
; 2. solution is #t
; 3. generative -> does the current 1string go to the next state
; 4. The final solution is the solution to the original problem

(check-expect (fsm-match? fsm-a-bc*-d "ad") #t)
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "acccccd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "da") #f)

(define (fsm-match? fsm s)
  (local (; FSM-State String -> Boolean
          (define (match? curstate s0)
            (cond
              [(and (string=? s0 "") (string=? curstate (fsm-final fsm))) #t]
              [else
               (local (; FSM-State [List-of 1Transition] -> FSM-State
                       (define (find-state l)
                         (cond
                           [(empty? l) #f]
                           [else
                            (if
                             (and
                              (string=? (transition-current (first l)) curstate)
                              (string=? (transition-key (first l)) (substring s0 0 1)))
                             (first l)
                             (find-state (rest l)))]))
                       (define state (find-state (fsm-transitions fsm))))
                 (if (boolean? state)
                     #f
                     (match? (transition-next state) (substring s0 1 (string-length s0)))))])))
    (match? (fsm-initial fsm) s)))

(define (fsm-match?.refactored fsm s)
  (local (; FSM-State String -> Boolean
          (define (match? curstate s0)
            (cond
              [(and (string=? s0 "") (string=? curstate (fsm-final fsm))) #t]
              [else
               (local ((define state
                         (foldl (lambda (t rst)
                                  (if (and
                                       (string=? (transition-current t) curstate)
                                       (string=? (transition-key t) (substring s0 0 1))) t rst))
                                #f (fsm-transitions fsm))))
                 (if (boolean? state) #f
                     (match? (transition-next state) (substring s0 1 (string-length s0)))))])))
    (match? (fsm-initial fsm) s)))

; Arrangements =========================================================


; COPIED FROM HTDP BOOK
; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
     (foldr (lambda (item others)
              (local ((define without-item
                        (arrangements (remove item w)))
                      (define add-item-to-front
                        (map (lambda (a) (cons item a)) without-item)))
                (append add-item-to-front others)))
            '()
            w)]))

; test
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
(check-satisfied (arrangements '("r" "a" "t")) all-words-from-rat?)
















