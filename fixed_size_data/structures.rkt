;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname structures) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define one-posn (make-posn 3 4))
(define p (make-posn 31 26))


; computes the distance of a-posn to the origin\
(check-expect (distance-to-0 (make-posn 0 5))5)
(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)
(define (distance-to-0 a-posn)
(sqrt
 (+ (sqr (posn-x a-posn))
    (sqr (posn-y a-posn)))))

;(distance-to-0 (make-posn 3 4))
;(distance-to-0 (make-posn 6 (* 2 4)))
;(+ (distance-to-0 (make-posn 12 5)) 10)

; computes the manhattan distance of a-posn to the origin
(check-expect (man-dist-0 (make-posn 0 5)) 5)
(check-expect (man-dist-0 (make-posn 7 0)) 7)
(check-expect (man-dist-0 (make-posn 7 5)) 12)
(define (man-dist-0 a-posn)
  (+ (posn-x a-posn) (posn-y a-posn)))

(define-struct entry [name phone email])
(define pl
(make-entry "Sarah Lee" "666-7771" "lee@classy-university.edu"))
(define bh
(make-entry "Tara Harper" "666-7770" "harper@small-college.edu"))
(entry-phone pl)
(entry-email bh)


(define-struct movie [title producer year])
(define-struct person [name hair eyes phone])
(define-struct pet [name number])
(define-struct CD [artist title price])
(define-struct sweater [material size producer])

(define m1
  (make-movie "harry" "jk" 1997))
(movie-year m1)

(define pe1
  (make-person "jon" "blue" "green" 1981298))
(person? pe1)

(define dog1
  (make-pet "doggo" 1))
(define cd1
  (make-CD "bon jovi" "hi" "5.99"))
(define sweater1
  (make-sweater "wool" "L" "kitty"))