;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname data_representation_acc) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Data Representations with Accumulators

(define-struct pair [head right])
; ConsOrEmpty is one of:
; - '()
; - (make-pair Any ConsOrEmpty)

; structure type definition
(define-struct cpair [count left right])

; A [MyList X] is one of:
; - '()
; - (make-cpair (tech "N") X [MyList X])
; accuulator: the count field is the number of cpairs in the list

; Any ConsOrEmpty -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    ;[(our-cons? a-list) (make-pair a-value a-list)]
    [else
     (error "cons: list as second argument expected")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first mimicked-list)
  (if (empty? mimicked-list)
      (error 'our-first "...")
      (pair-head mimicked-list)))

;ConsOrEmpty -> ConsOrEmpty
(define (our-rest mimicked-list)
  (if (empty? mimicked-list)
      '()
      (pair-right mimicked-list)))

; Any [MyList X] -> [MyList X]
; data definitions, via a constructor-function
(define (our-cons.v2 f r)
  (cond
    [(empty? r) (make-cpair 1 f r)]
    [(cpair? r) (make-cpair (+ (cpair-count r) 1) f r)]
    [else (error 'cons
                 "second argument must be a list, but ...")]))

; Any -> N
; how many items does l contain
(define (our-length l)
  (cond
    [(empty? l) 0]
    [(cpair? l) (cpair-count l)]
    [else (error 'my-length "expects a list, given ...")]))


; JUNGLE =====================================================

; OnOff is [List Number Number]
; interpretation [List On Off]
; number of On, number of Off

; RorL is one of:
; - 'right
; - 'left

(define-struct pstate [init inter final])
; PuzzleState is a structure:
; (make-pstate OnOff RorL OnOff)
; interpretation (make-pstate l b r)
; l is onoff number of the left, r is right
; b is the location of the boat

(define-struct pstate.v2 [init inter final acc])
; PuzzleState.v2 is a structure:
; (make-pstate OnOff RorL OnOff [List-of PuzzleState.v2])
; interpretation (make-pstate l b r a)
; l is onoff number of the left, r is right
; b is the location of the boat
; a is the sequence of states traversed to
; get to the current state

(define UNIT 50)
(define BOATUNIT (* 2 UNIT))
(define ON (circle UNIT 'solid 'black))
(define OFF (circle UNIT 'outline 'black))
(define BOATR (beside (rectangle BOATUNIT BOATUNIT 'solid 'black)
                      (rectangle BOATUNIT BOATUNIT 'outline 'black)))
(define BOATL (beside (rectangle BOATUNIT BOATUNIT 'outline 'black)
                      (rectangle BOATUNIT BOATUNIT 'solid 'black)))

; PuzzleState -> Boolean
; is it the final state

(check-expect (final? (make-pstate '(0 0) 'right '(3 3))) #t)

(define (final? p)
  (= 3 (first (pstate-final p))
     (second (pstate-final p))))

(check-expect (final?.v2 (make-pstate.v2 '(0 0) 'right '(3 3) '())) #t)

(define (final?.v2 p)
  (= 3 (first (pstate.v2-final p))
     (second (pstate.v2-final p))))

; PuzzleState -> Image
; render image
(define (render-mc p)
  (local ((define (render-circles i c)
            (cond
              [(zero? c) (empty-scene 1 1)]
              [else
               (beside i (render-circles i (sub1 c)))]))
          (define (render-boat b)
            (if (symbol=? b 'left)
                BOATR BOATL)))
    (beside (render-circles ON (first (pstate-init p)))
            (render-circles OFF (second (pstate-init p)))
            (render-boat (pstate-inter p))
            (render-circles ON (first (pstate-final p)))
            (render-circles OFF (second (pstate-final p))))))

; [List-of PuzzleState] -> [List-of PuzzleState]
; generate all possible next states

(check-expect (create-next-states
               '()) '())

(define (create-next-states alop)
  (cond
    [(empty? alop) '()]
    [else
     (append
      (next-states (first alop))
      (create-next-states (rest alop)))]))


; PuzzleState -> [List-of PuzzleState]
; create list of possible next states

(check-expect (next-states (make-pstate '(6 6) 'left '(0 0)))
              (list
               (make-pstate '(6 4) 'right '(0 2))
               (make-pstate '(6 5) 'right '(0 1))
               (make-pstate '(5 5) 'right '(1 1))))
(check-expect (next-states (make-pstate '(3 4) 'right '(3 2)))
              (list
               (make-pstate '(4 4) 'left '(2 2))))
(check-expect (next-states (make-pstate '(6 6) 'right '(0 0)))
              '())
(check-expect (next-states (make-pstate '(2 2) 'left '(1 1)))
              (list
               (make-pstate '(0 2) 'right '(3 1))
               (make-pstate '(1 1) 'right '(2 2))))
(check-expect (next-states (make-pstate '(1 1) 'left '(2 2)))
              (list
               (make-pstate '(0 1) 'right '(3 2))
               (make-pstate '(0 0) 'right '(3 3))))

(define (next-states s)
  (local ((define (left-one s)
            (first (pstate-init s)))
          (define (left-two s)
            (second (pstate-init s)))
          (define (right-one s)
            (first (pstate-final s)))
          (define (right-two s)
            (second (pstate-final s)))
          (define (main s0)
            (cond
              [(symbol=? (pstate-inter s) 'left)
               (list
                (make-pstate
                 (list (left-one s) (- (left-two s) 2)) 'right
                 (list (right-one s) (+ (right-two s) 2)))
                (make-pstate
                 (list (- (left-one s) 2) (left-two s)) 'right
                 (list (+ (right-one s) 2) (right-two s)))
                (make-pstate
                 (list (left-one s) (- (left-two s) 1)) 'right
                 (list (right-one s) (+ (right-two s) 1)))
                (make-pstate
                 (list (- (left-one s) 1) (left-two s)) 'right
                 (list (+ (right-one s) 1) (right-two s)))
                (make-pstate
                 (list (- (left-one s) 1) (- (left-two s) 1)) 'right
                 (list (+ (right-one s) 1) (+ (right-two s) 1))))]
              [else
               (list
                (make-pstate
                 (list (left-one s) (+ (left-two s) 2)) 'left
                 (list (right-one s) (- (right-two s) 2)))
                (make-pstate
                 (list (+ (left-one s) 2) (left-two s)) 'left
                 (list (- (right-one s) 2) (right-two s)))
                (make-pstate
                 (list (left-one s) (+ (left-two s) 1)) 'left
                 (list (right-one s) (- (right-two s) 1)))
                (make-pstate
                 (list (+ (left-one s) 1) (left-two s)) 'left
                 (list (- (right-one s) 1) (right-two s)))
                (make-pstate
                 (list (+ (left-one s) 1) (+ (left-two s) 1)) 'left
                 (list (- (right-one s) 1) (- (right-two s) 1))))])))
    (filter (lambda (x) (and (not (negative? (left-one x)))
                             (not (negative? (left-two x)))
                             (not (negative? (right-one x)))
                             (not (negative? (right-two x)))
                             (or
                              (zero? (left-one x))
                              (>= (left-one x) (left-two x)))
                             (or
                              (>= (right-one x) (right-two x))
                              (zero? (right-one x)))))
            (main s))))

; PuzzleState -> PuzzleState
; determine whether the final state is reachable from
; the given state
; generative: create search tree of possible boat rides
; termination: As long as there is an answer, the bfs nature
; of the search will find the answer and terminate.
; if there is none, then it will not terminate

(check-expect (solve (make-pstate '(3 3) 'left '(0 0))) (make-pstate '(0 0) 'right '(3 3)))

(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generate the successor states
          ; for all intermediate ones
          (define (solve* los)
            (cond
              [(ormap final? los) (first (filter final? los))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))




; [List-of PuzzleState.v2] -> [List-of PuzzleState.v2]
; generate all possible next states

(check-expect (create-next-states.v2
               (list (make-pstate.v2 '(3 3) 'left '(0 0) '())))
              (list
               (make-pstate.v2 '(3 1) 'right '(0 2)
                               (list (make-pstate.v2 '(3 3) 'left '(0 0) '())))
               (make-pstate.v2 '(3 2) 'right '(0 1)
                               (list
                                (make-pstate.v2 '(3 3) 'left '(0 0) '())))
               (make-pstate.v2 '(2 2) 'right '(1 1)
                               (list
                                (make-pstate.v2 '(3 3) 'left '(0 0) '())))))

(define (create-next-states.v2 alop0)
  (local (; [List-of PuzzleState.v2] ??? -> [List-of PuzzleState.v2]
          (define (create-next-states/a alop)
            (cond
              [(empty? alop) '()]
              [else
               (append
                (next-states.v2 (first alop))
                (create-next-states/a (rest alop)))])))
    (create-next-states/a alop0)))

; PuzzleState.v2 -> [List-of PuzzleState.v2]
; create list of possible next states

(check-expect (next-states.v2 (make-pstate.v2 '(3 3) 'left '(0 0) (list
                                                                   (make-pstate.v2
                                                                    (list 3 1)
                                                                    'right
                                                                    (list 0 2)
                                                                    '()))))
              (list
               (make-pstate.v2
                (list 3 2)
                'right
                (list 0 1)
                (list
                 (make-pstate.v2
                  (list 3 3)
                  'left
                  (list 0 0)
                  '())
                 (make-pstate.v2
                  (list 3 1)
                  'right
                  (list 0 2)
                  '())))
               (make-pstate.v2
                (list 2 2)
                'right
                (list 1 1)
                (list
                 (make-pstate.v2
                  (list 3 3)
                  'left
                  (list 0 0)
                  '())
                 (make-pstate.v2
                  (list 3 1)
                  'right
                  (list 0 2)
                  '())))))
;(check-expect (next-states.v2 (make-pstate.v2 '(3 4) 'right '(3 2) '()) '())
;              (list
;               (make-pstate.v2 '(4 4) 'left '(2 2) '())))
;(check-expect (next-states.v2 (make-pstate.v2 '(6 6) 'right '(0 0) '()) '())
;              '())
;(check-expect (next-states.v2 (make-pstate.v2 '(2 2) 'left '(1 1) '()) '())
;              (list
;               (make-pstate.v2 '(0 2) 'right '(3 1) '())
;               (make-pstate.v2 '(1 1) 'right '(2 2) '())))
;(check-expect (next-states.v2 (make-pstate.v2 '(1 1) 'left '(2 2) '()) '())
;              (list
;               (make-pstate.v2 '(0 1) 'right '(3 2) '())
;               (make-pstate.v2 '(0 0) 'right '(3 3) '())))

(define (next-states.v2 s0)
  (local (; PuzzleState.v2 Number Number Symbol -> PuzzleState.v2
          (define (left-one s)
            (first (pstate.v2-init s)))
          (define (left-two s)
            (second (pstate.v2-init s)))
          (define (right-one s)
            (first (pstate.v2-final s)))
          (define (right-two s)
            (second (pstate.v2-final s)))
          
          (define (change s1 n m side)
            (make-pstate.v2
             (list ((if (symbol=? 'right side) - +) (left-one s1) n)
                   ((if (symbol=? 'right side) - +) (left-two s1) m))
             side
             (list ((if (symbol=? 'right side) + -) (right-one s1) n)
                   ((if (symbol=? 'right side) + -) (right-two s1) m))
             (cons (normalize s1) (pstate.v2-acc s0))))
          
          (define (validate x)
            (and (>= (left-one x) 0)
                 (>= (left-two x) 0)
                 (>= (right-one x) 0)
                 (>= (right-two x) 0)
                 (or
                  (zero? (left-one x))
                  (>= (left-one x) (left-two x)))
                 (or
                  (>= (right-one x) (right-two x))
                  (zero? (right-one x)))
                 (not (member? (normalize x) (pstate.v2-acc s0)))))
          
          (define candidates
            (cond
              [(symbol=? (pstate.v2-inter s0) 'left)
               (list
                (change s0 0 2 'right)
                (change s0 2 0 'right)
                (change s0 0 1 'right)
                (change s0 1 0 'right)
                (change s0 1 1 'right))]
              [else
               (list
                (change s0 0 2 'left)
                (change s0 2 0 'left)
                (change s0 0 1 'left)
                (change s0 1 0 'left)
                (change s0 1 1 'left))])))
    (filter validate candidates)))

; PuzzleState.v2 -> PuzzleState.v2
; remove the accumulator info
(define (normalize r)
  (make-pstate.v2
   (pstate.v2-init r)
   (pstate.v2-inter r)
   (pstate.v2-final r)
   '()))

; PuzzleState.v2 -> [List-of PuzzleState.v2]
; create list of states from initial to final
; generative: create search tree of possible boat rides
; termination: ???

(check-expect (solve.v2 (make-pstate.v2 '(3 3) 'left '(0 0) '()))
              (list
               (make-pstate.v2
                (list 0 0)
                'right
                (list 3 3)
                '())
               (make-pstate.v2
                (list 0 2)
                'left
                (list 3 1)
                '())
               (make-pstate.v2
                (list 0 1)
                'right
                (list 3 2)
                '())
               (make-pstate.v2
                (list 0 3)
                'left
                (list 3 0)
                '())
               (make-pstate.v2
                (list 0 2)
                'right
                (list 3 1)
                '())
               (make-pstate.v2
                (list 2 2)
                'left
                (list 1 1)
                '())
               (make-pstate.v2
                (list 1 1)
                'right
                (list 2 2)
                '())
               (make-pstate.v2
                (list 3 1)
                'left
                (list 0 2)
                '())
               (make-pstate.v2
                (list 3 0)
                'right
                (list 0 3)
                '())
               (make-pstate.v2
                (list 3 2)
                'left
                (list 0 1)
                '())
               (make-pstate.v2
                (list 3 1)
                'right
                (list 0 2)
                '())
               (make-pstate.v2
                (list 3 3)
                'left
                (list 0 0)
                '())))

(define (solve.v2 state0)
  (local (; [List-of PuzzleState.v2] -> [List-of PuzzleState.v2]
          ; generative generate the successor states
          ; for all intermediate ones
          (define (solve* los)
            (cond
              [(ormap final?.v2 los)
               (local ((define res (first (filter final?.v2 los))))
                 (cons (normalize res) (pstate.v2-acc res)))]
              [else
               (solve* (create-next-states.v2 los) )])))
    (solve* (list state0))))

; PuzzleState.v2 -> Image
; render image
(define (render-mc.v2 p)
  (local ((define (render-circles i c)
            (cond
              [(zero? c) (empty-scene 1 1)]
              [else
               (beside i (render-circles i (sub1 c)))]))
          (define (render-boat b)
            (if (symbol=? b 'left)
                BOATR BOATL)))
    (beside (render-circles ON (first (pstate.v2-init p)))
            (render-circles OFF (second (pstate.v2-init p)))
            (render-boat (pstate.v2-inter p))
            (render-circles ON (first (pstate.v2-final p)))
            (render-circles OFF (second (pstate.v2-final p))))))

(run-movie 1 (map render-mc.v2
                  (reverse
                   (solve.v2 (make-pstate.v2 '(3 3) 'left '(0 0) '())))))



















