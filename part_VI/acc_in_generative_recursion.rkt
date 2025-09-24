;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname acc_in_generative_recursion) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; In Generative Recursion



; A SimpleGraph is a [List-of Connection]
; interpretation: each node has exactly one one-directional
; connection to another node, possibly itself

; A Connection is a list of two items:
; (list Node Node)

; A Node is a Symbol

(define a-simple-graph
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

; Node Node SimpleGraph -> Boolean
; is there a path from origination to destination in sg

; 4 questions of generative recursion:
; 1. trivial -> origination is destination
; 2. return #t
; 3. generative: go through the list of candidates
; each candidate is an instance of a small orig to dest.
; 4. true if there is at least one successful path. so kind of
; yes, the solution to the new problem is the answer

(check-expect (path-exists?/a 'A 'E a-simple-graph) #t)
(check-expect (path-exists?/a 'A 'F a-simple-graph) #f)

(define (path-exists?/a origination destination sg)
  (local (; Node Node SimpleGraph [List-of Node] -> Boolean
          ; assume the nodes in seen are known not to solve the problem
          (define (with-acc o seen)
            (cond
              [(symbol=? o destination) #t]
              [(member? o seen) #f]
              [else
               (with-acc
                   (neighbor o sg) (cons o seen))])))
    (with-acc origination '())))

(define (path-exists?.v1 origination destination sg)
  (cond
    [(symbol=? origination destination) #t]
    [else
     (path-exists?.v1
      (neighbor origination sg) destination sg)]))

; Node SimpleGraph -> Node
; determine the node that is connected to a-node in sg

(check-expect (neighbor 'A a-simple-graph) 'B)
(check-error
 (neighbor 'G a-simple-graph) "neighbor: not a node")

(define (neighbor a-node sg)
  (cond
    [(empty? sg) (error "neighbor: not a node")]
    [else (if (symbol=? (first (first sg)) a-node)
              (second (first sg))
              (neighbor a-node (rest sg)))]))

; Example from Part V ======================================================


(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

(define cyclic-graph
  '((A B E)
    (B E F)
    (E C F)
    (C B D)
    (F D G)
    (D)
    (G)
    ))
(define c0
  '((A B)
    (B A)
    (C)))

; Node Graph -> [List-of Node]
; list of immediate neighbors

(check-expect (listofneighbors 'a '((a b c))) '(b c)) 

(define (listofneighbors n g)
  (rest (first (filter (lambda (p) (equal? (first p) n)) g))))

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

; 4 questions of generative recursion
; 1. trivial -> the destination is the origin
; 2. solution is '(destination)
; 3. generate -> picking one neighbor generates a new
; instance of a find-a-path problem
; 4. the solution is consed together, add the origin to destination.

(check-member-of (find-path 'E 'D sample-graph) '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph) #f)
(check-expect (find-path 'A 'C c0) #f)

(check-expect (find-path 'E 'G sample-graph) '(E F G))


(define (find-path origination destination G)
  (local (; Node [List-of Node] -> Boolean
          (define (find-path/a o seen)
            (cond
              [(symbol=? o destination) (list destination)]
              [(member? o seen) #f]
              [else (local ((define next (listofneighbors o G))
                            (define candidate
                              (find-path/list next (cons o seen))))
                      (cond
                        [(boolean? candidate) #f]
                        [else (cons o candidate)]))]))
          
          ; [List-of Node] [List-of Node] -> [Maybe Path]
          (define (find-path/list loo seen0)
            (cond
              [(empty? loo) #f]
              [else (local ((define candidate
                              (find-path/a (first loo) seen0)))
                      (cond
                        [(boolean? candidate)
                         (find-path/list (rest loo) seen0)]
                        [else candidate]))])))
    (find-path/a origination '())))






















