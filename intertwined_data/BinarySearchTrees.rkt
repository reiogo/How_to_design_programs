;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname BinarySearchTrees) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Binary Search Trees
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

(make-node 15 'd NONE (make-node 24 'i NONE NONE))

(make-node 15 'd (make-node 87 'h NONE NONE) NONE)

; Number BT -> Boolean
; whether the ssn n is in bt

(check-expect
 (contains-bt? 4 (make-node 15 'd (make-node 4 'j NONE NONE)
                            NONE)) #t)

(check-expect
 (contains-bt? 4 (make-node 15 'd (make-node 5 'j NONE NONE)
                            NONE)) #f)
 

(define (contains-bt? n bt)
  (cond
    [(no-info? bt) #f]
    [else
     (or
      (= (node-ssn bt) n)
      (contains-bt? n (node-left bt))
      (contains-bt? n (node-right bt))
      )]))

; Number BT -> [Maybe String]
; searches for the ssn in bt and returns name or #f

(check-expect
 (search-bt 15 (make-node 15 'd (make-node 5 'j NONE NONE)
                          NONE)) 'd)
(check-expect
 (search-bt 16 (make-node 15 'd (make-node 5 'j NONE NONE)
                          NONE)) #f)

(check-expect
 (search-bt 16 (make-node 15 'd (make-node 5 'j NONE
                                           (make-node 16 'h NONE NONE))
                          NONE))
 'h)

(define (search-bt n bt)
  (cond
    [(no-info? bt) #f]
    [(= (node-ssn bt) n) (node-name bt)]
    [else
     (local ((define left (search-bt n (node-left bt)))
             (define right (search-bt n (node-right bt))))
       (cond
         [(not (boolean? left)) left]
         [(not (boolean? right)) right]
         [else #f]))]))


; A BST is defined by whether all nodes in the left tree
; are smaller, and all are larger in the right tree

; BT -> [List-of Number]
; produces the inorder sequence of the tree

(check-expect
 (inorder
  (make-node 10 'a
             (make-node 5 'b NONE NONE)
             (make-node 15 'c NONE NONE)))
 '(5 10 15))

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (append
      (inorder (node-left bt))
      (list (node-ssn bt))
      (inorder (node-right bt)))]))

; Number BST -> [String or None]
; search bst for ssn number

(check-expect
 (search-bst 15 (make-node 10 'a
                           (make-node 5 'b NONE NONE)
                           (make-node 15 'c NONE NONE)))
 'c)
(check-expect
 (search-bst 16 (make-node 10 'a
                           (make-node 5 'b NONE NONE)
                           (make-node 15 'c NONE NONE)))
 NONE)

(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [(= (node-ssn bst) n) (node-name bst)]
    [else
     (cond
       [(> n (node-ssn bst)) (search-bst n (node-right bst))]
       [(< n (node-ssn bst)) (search-bst n (node-left bst))])]))

; BST Number Symbol -> BST
; add a new node to a bst

(check-expect
 (insert-bst
  (make-node 10 'a
             (make-node 5 'b NONE NONE)
             (make-node 15 'c NONE NONE))
  16 'd)
 (make-node 10 'a
            (make-node 5 'b NONE NONE)
            (make-node 15 'c NONE
                       (make-node 16 'd NONE NONE))))
(check-expect
 (insert-bst
  (make-node 10 'a
             (make-node 5 'b NONE NONE)
             (make-node 15 'c NONE NONE))
  1 'd)
 (make-node 10 'a
            (make-node 5 'b (make-node 1 'd NONE NONE) NONE)
            (make-node 15 'c NONE NONE)))

(check-expect
 (insert-bst
  NONE 10 'a)
 (make-node 10 'a NONE NONE))
 

(define (insert-bst bst n s)
  (cond
    [(no-info? bst) (make-node n s NONE NONE)]
    [else
     (cond
       [(< n (node-ssn bst))
        (make-node
         (node-ssn bst)
         (node-name bst)
         (insert-bst (node-left bst) n s)
         (node-right bst))]
       [(> n (node-ssn bst))
        (make-node
         (node-ssn bst)
         (node-name bst)
         (node-left bst)
         (insert-bst (node-right bst) n s))])]))

; [List-of [List-of Number Symbol]] -> BST
; create a bst from a list of numbers and symbols

(check-expect
 (create-bst-from-list '((1 z) (5 a) (10 b) (15 c)))
 (make-node 15 'c
            (make-node 10 'b 
                       (make-node 5 'a
                                  (make-node 1 'z NONE NONE) NONE) NONE) NONE))
(define sample
  '((99 o)
    (77 l)
    (24 i)
    (10 h)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))


(define (create-bst-from-list llons)
  (cond
    [(empty? llons) NONE]
    [else
     (insert-bst 
      (create-bst-from-list (rest llons))
      (first (first llons)) (second (first llons)))]))



