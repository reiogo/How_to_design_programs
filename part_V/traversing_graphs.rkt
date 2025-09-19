;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname traversing_graphs) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Traversing Graphs

; A Node is a Symbol

; A Graph is one of:
; - '()
; - (cons [List-of Node] Graph)

; A Path is a [List-of Node].
; interpretation: The list of nodes specifies a sequence
; of immediate neighbors that leads from the first Node on
; the list to the last one.

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

; Node Graph -> [List-of Node]
; list of immediate neighbors

(check-expect (neighbors 'a '((a b c))) '(b c)) 

(define (neighbors n g)
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
(check-expect (find-path 'E 'G sample-graph) '(E F G))


(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #f]
              [else (cons origination candidate)]))]))


; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #f]
    [else (local ((define candidate (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate) (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; Graph -> Boolean
; attempts to find a path between all pairs of nodes in g

(check-expect (test-on-all-nodes sample-graph) #f)
;(check-expect (test-on-all-nodes cyclic-graph) #f)
(check-expect (test-on-all-nodes '((a b) (b a))) #t)

(define (test-on-all-nodes g)
  (cond
    [(empty? g) #t]
    [else
     (and
      (local ((define cur (first (first g))))
        (foldr (lambda (n rst)
                 (and
                  (not (false? (find-path cur (first n) g)))
                  rst)) #t g))
      (test-on-all-nodes (rest g)))]))


; Refactor find-path
(check-member-of (find-path.v1 'E 'D sample-graph) '(E F D) '(E C D))
(check-expect (find-path.v1 'C 'G sample-graph) #f)
(check-expect (find-path.v1 'E 'G sample-graph) '(E F G))


(define (find-path.v1 origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (foldl (lambda (o rst)
                             (local ((define candidate0 (find-path.v1 o destination G)))
                               (cond
                                 [(boolean? candidate0) rst]
                                 [else candidate0]))) #f next)))
            (cond
              [(boolean? candidate) #f]
              [else (cons origination candidate)]))]))






















