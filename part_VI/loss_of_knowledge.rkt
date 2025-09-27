;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname loss_of_knowledge) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Loss of Knowledge

; Structural Processing ========================================

; [List-of Number -> [List-of Number]
; convert a list of relative distances to a list
; of absolute distances
; the first item on the list represents the distance to the origin

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))

; Number [List-of Number] -> [List-of Number]
; add n to each number on l

(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

(define (add-to-each.v1 n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))

(define (add-to-each n l)
  (map (lambda (l0) (+ n l0)) l))

(define (relative->absolute/a l accu-dist)
  (cond
    [(empty? l) '()]
    [else (local ((define tally (+ (first l) accu-dist)))
            (cons tally
                  (relative->absolute/a (rest l) tally)))]))

; [List-of Number] -> [List-of Number]
; convert a list of relative distances to a list of absolute
; distances. The first item on the list represents the
; distance to the origin

(check-expect (relative->absolute.v2 '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative->absolute.v2 l0)
  (local (; [List-of Number] Number -> [List-of Number]
          (define (relative->absolute/a l accu-dist)
            (cond
              [(empty? l) '()]
              [else
               (local ((define accu (+ (first l) accu-dist)))
                 (cons accu (relative->absolute/a (rest l) accu)))])))
    (relative->absolute/a l0 0)))

(define (test-speed f size)
  (time (f (build-list size add1))))

; (test-speed relative->absolute 7000)
; (test-speed relative->absolute.v2 7000)
; (relative->absolute '(1 2 3))

; [List-of Number] -> [List-of Number]

(check-expect (relative->absolute.v3 '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative->absolute.v3 l)
  (reverse (foldr (lambda (f l) (cons (+ f (first l)) l))
                  (list (first l))
                  (reverse (rest l)))))

; [List-of Any] -> [List-of Any]
; reverses a list

(check-expect (myreverse '(1 2 3)) '(3 2 1))

(define (myreverse l)
  (cond
    [(empty? l) '()]
    [else
     (append
      (myreverse (rest l))
      (list (first l)))]))

(check-expect (myreverse-acc '(1 2 3)) '(3 2 1))

(define (myreverse-acc l)
  (local ((define (r/a l acc)
            (cond
              [(empty? l) acc]
              [else
               (r/a (rest l) (cons (first l) acc))])))
    (r/a l '())))












