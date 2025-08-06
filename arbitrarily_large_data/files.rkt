;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname files) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define DIR "readfile/")
(define FILE (string-append DIR "ttt.txt"))

; (read-file FILE)

; A List-of-string is one of:
; - '()
; - (cons String list-of-string)
; interpretation a list of strings
; (read-lines FILE)
; (read-words FILE)

; A List-of-list-of-string
; - '()
; - (cons list-of-string list-of-list-of-string)
; interpretation a list of list of strings
(read-words/line FILE)

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
(define line2 (cons "a" (cons "bon" '())))


(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line1 (cons line2 '())))

; List-of-numbers -> Number
; counts the number of words on los
(define (words alon)
  (cond
    [(empty? alon) ...]
    [(cons? alon) (... (first alon)
                       (line-processor (rest alon)))]))


; LLS -> List-of-numbers
; determine the number of words on each line

(check-expect
 (words-on-line lls0) '())
(check-expect
 (words-on-line lls1) (cons 2 (cons 0 '())))

               
(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [(cons? lls)
     (cons (length (first lls))
           (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the number of words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))

; A list-of-lines is one of:
; - '()
; - (cons string list-of-lines)
; interpretation a list of strings that represent lines

; STRING-JOIN
; take a list of strings and join them with a given string (like ", ")
(check-expect (string-join line0 " ") "hello world")
(define (string-join alos sep)
  (cond
    [(empty? alos) ""]
    [(empty? (rest alos)) (first alos)]
    [(cons? alos)
     (string-append (first alos) sep
                    (string-join (rest alos) sep))]))


; COLLAPSE
; LLS -> String
; converts an lls into one string where the words are space
; separated and the lines are newline separated
(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "hello world\n")

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [(empty? (rest lls)) (string-join (first lls) " ")]
    [(cons? lls)
     (string-append
      (string-join (first lls) " ")
      "\n"
      (collapse (rest lls)))]))

;(write-file "readfile/ttt.dat" (collapse (read-words/line FILE)))

; los -> los
; removes articles a, an, and the from a los
(check-expect (no-articles line1) '())
(check-expect (no-articles line2) (cons "bon" '()))
(check-expect (no-articles
               (cons "an"
                     (cons "bon" '())))
              (cons "bon" '()))
(check-expect (no-articles
               (cons "the"
                     (cons "bon" '())))
              (cons "bon" '()))
(check-expect (no-articles line0) (cons "hello"
                                        (cons "world" '())))
(define (no-articles alos)
  (cond
    [(empty? alos) '()]
    [(cons? alos)
     (cond
       [(string=? (first alos) "a") (no-articles (rest alos))]
       [(string=? (first alos) "an") (no-articles (rest alos))]
       [(string=? (first alos) "the") (no-articles (rest alos))]
       [else
        (cons
         (first alos)
         (no-articles (rest alos)))])]))

; LLS -> LLS
; remove articles a, an, and the from an lls
(check-expect (remove-articles lls0) '())
(check-expect (remove-articles lls2)
              (cons '()
                    (cons (cons "bon" '())
                          '())))

(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [(cons? lls)
     (cons (no-articles (first lls)) (remove-articles (rest lls)))]))

; File -> File
; removes articles a, an, and the from a file and writes out a
; new file named "no-articles-" appended
; with the original file name

(define (article-remover n)
  (write-file
   (string-append DIR "no-articles-" n)
   (collapse
    (remove-articles
     (read-words/line
      (string-append
       DIR n))))))
(article-remover "ttt.txt")




; 1String -> String
; converts the given 1String into a three-letter numberic String

(check-expect (encode-letter "\t") (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a") (string-append "0" (code1 "a")))
(check-expect (encode-letter "z") (code1 "z"))

(define (encode-letter s)
  (cond
    [(< (string->int s) 10) (string-append "00" (code1 s))]
    [(< (string->int s) 100) (string-append "0" (code1 s))]
    [else (code1 s)]))

; 1String -> String
; convert the given 1String into a String

(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))

; String-> String
; encodes a string with ascii
(check-expect
 (encode-string "ab")
 (string-append
  (encode-letter "a")
  (encode-letter "b")
  ))
(define (encode-string s)
  (cond
    [(string=? "" s) ""]
    [else
     (string-append
      (encode-letter (substring s 0 1))
      (encode-string
       (substring
        s
        1
        (string-length s))))]))

; Los -> String
; encodes a list of strings with ascii
(check-expect
 (encode-los (cons "ab" '()))
 (string-append
  (encode-letter "a")
  (encode-letter "b")
  ))
(define (encode-los alos)
  (cond
    [(empty? alos) ""]
    [(cons? alos)
     (string-append
      (encode-string (first alos))
      (encode-los (rest alos)))]))

; LLS -> String
; encode an lls numerically with ascii and numbers from 0 to 256
(check-expect
 (encode-numeric
  (cons
   (cons "ab\n" '())
   '())) (string-append
          (encode-letter "a")
          (encode-letter "b")
          (encode-letter "\n")))
(define (encode-numeric lls)
  (cond
    [(empty? lls) ""]
    [(cons? lls)
     (string-append (encode-los (first lls)) (encode-numeric (rest lls)))]))

; FILE -> FILE
; encode a file and save it to "encoded-*filename*"
(define (encode-file n)
  (write-file
   (string-append DIR "encoded-" n)
   (encode-numeric
    (read-words/line
     (string-append DIR n)))))


; (encode-file "ttt.txt")

; A Matrix is one of:
; - (cons Row '())
; - (cons Row Matrix)
; contraint all rows in matrix are of the same length

; A Row is a one of:
; - '()
; - (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(cons
 (cons 11
       (cons 12 '()))
 (cons
  (cons 21
        (cons 22 '()))
  '()))

(define row3 (cons 11 (cons 21 '())))
(define row4 (cons 12 (cons 22 '())))
(define t-mat1 (cons row3 (cons row4 '())))

; Matrix -> Matrix
; transpose teh items on the givcen matrix along the diagonal
(check-expect (transpose mat1) t-mat1)
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; FIRST*
; LLN -> LoN
; find the first lon from a given lln

(check-expect
 (first* mat1)
 (cons
  (first row1)
  (cons 
   (first row2) '())))
              
(define (first* lln)
  (cond
    [(empty? lln) '()]
    [(cons? lln)
     (cons (first (first lln)) (first* (rest lln)))]))

; REST*
; LLN -> LLN

(check-expect
 (rest* mat1)
 (cons
  (rest row1)
  (cons 
   (rest row2) '())))

 (define (rest* lln)
   (cond
     [(empty? lln) '()]
     [(cons? lln)
      (cons (rest (first lln))
            (rest* (rest lln)))]))


















 