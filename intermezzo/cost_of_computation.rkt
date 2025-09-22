;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cost_of_computation) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Cost of computation

; A NumberTree is one of
; - Number
; - (list NumberTree NumberTree)

; NumberTree -> Number
; sum the numbers in a number tree
 
(check-expect (sum-tree '(3 (4 3))) 10)
(check-expect (sum-tree 0) 0)
(check-expect (sum-tree '(((3 2) 3) 3)) 11)

(define (sum-tree nt)
  (cond
    [(number? nt) nt]
    [else
     (+ (sum-tree (first nt)) (sum-tree (second nt)))]))

; abstract runtime is order of n where
; n is the number of nodes in the tree
; acceptable measure? its height?
; best possible shape is a balanced tree
; worst possible shape is a lopsided tree

; Number [List-of Number] -> Boolean
; is x in l

(check-expect (searchL 0 '(3 2 1 0)) #t)
(check-expect (searchL 4 '(3 2 1 0)) #f)

(define (searchL x l)
  (cond
    [(empty? l) #false]
    [else
     (or (= (first l) x)
         (searchL x (rest l)))]))

(define (searchS x l)
  (cond
    [(= (length l) 0) #f]
    [else
     (or (= (first l) x)
         (searchS x (rest l)))]))



; N -> [List Booelan Boolean]
; how long do searchS and searchL take
; to look for n in (list 0 ... (-n 1))
(define (timing n)
  (local ((define long-list (build-list n (lambda (x) x))))
    (list
     (time (searchS n long-list))
     (time (searchL n long-list )))))

(timing 10000)
(timing 20000)
(timing 30000)



















