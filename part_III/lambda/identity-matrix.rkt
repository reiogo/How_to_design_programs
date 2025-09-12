; Number -> [List-of [List-of 1or0]]
; make an identity matrix where the row/column value is n

(check-expect
 (identity-matrix 1)
 '((1)))

(check-expect
 (identity-matrix 2)
 '((1 0) (0 1)))

(check-expect
 (identity-matrix 3)
 '((1 0 0) (0 1 0) (0 0 1)))

(define (identity-matrix n)
  (build-list
   n
   (lambda (n0)
     (build-list
      n
      (lambda (n1) (if (= n1 n0) 1 0))))))
