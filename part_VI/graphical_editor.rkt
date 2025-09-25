;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graphical_editor) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Graphical Editor, with Mouse

(define FONT-SIZE 16)
(define FONT-COLOR "black")

; [List-of 1String] -> Image
; render a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define-struct editor [pre post])

; An Editor is a structure:
; (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of an
; interactive editor, (reverse p) corresponds to the text to the
; left of the cursor and s to the one on its right

; [List-of 1String] N -> Editor
; produces a make editor based on the x-position from x

(check-expect (split-structural '("s" "h" "e") 400)
              (make-editor "she" ""))
(check-expect (split-structural '("s" "h" "e") 2)
              (make-editor "" "she"))
(check-expect (split-structural '("s" "h" "e") 20)
              (make-editor "sh" "e"))

(define (split-structural ed0 x)
  (local (; [List-of 1String] [List-of 1String] Number -> Editor
          ; accumulator: a represents the prefix of ed
          ; accumulator: d represents the width of letters
          ; in ed0 but not in ed
          (define (split/a ed a d)
            (cond
              [(empty? ed) (make-editor (implode (reverse a)) "")]
              [(<= d x
                   (+ d (image-width
                         (editor-text (list (first ed))))))
               (make-editor (implode (reverse a)) (implode ed))]
              [else
               (split/a (rest ed)
                        (cons (first ed) a)
                        (+ (image-width
                            (editor-text (list (first ed)))) d))])))
    (split/a ed0 '() 0)))






; INTEGRATE INTO OLD GRAPHICAL EDITOR ===========================


; CONSTRAINTS
(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
;(define FONT-SIZE 16) ; the font size
;(define FONT-COLOR "black") ; the font color

; GRAPHICAL CONSTANTS
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; EDITOR
; (define-struct editor [pre post])
; An Editor is a structure:
; (make-editor Lo1S Lo1s)
; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; data example 1:
;(make-editor all good)

; data example 2:
;(make-editor lla good)

; REV
; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect
 (rev (cons "a" (cons "b" (cons "c" '()))))
 (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else
     (ins
      (first l)
      (rev (rest l)))]))

;INS
; 1String Lo1S -> Lo1S
; insert a s at the end of a alo1s
(check-expect (ins "a" (cons "b" '()))
              (cons "b" (cons "a" '())))
(check-expect (ins "a" (cons "m" (cons "b" '())))
              (cons "m" (cons "b" (cons "a" '()))))
(define (ins s alo1s)
  (cond
    [(empty? alo1s) (cons s '())]
    [else (cons
           (first alo1s)
           (ins s (rest alo1s)))]))

; CREATE-EDITOR
; String String -> Editor
(check-expect
 (create-editor "" "")
 (make-editor '() '()))
(check-expect
 (create-editor "ab" "df")
 (make-editor
  (cons "b"
        (cons "a" '()))
  (cons "d"
        (cons "f" '()))))
  
(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))

;MY-IMPLODE
; lo1s -> string
(check-expect (my-implode (cons "a" '())) "a")
(check-expect (my-implode (cons "a" (cons "a" '()))) "aa")
(define (my-implode alo1s)
  (cond
    [(empty? alo1s) ""]
    [else
     (string-append
      (first alo1s)
      (my-implode (rest alo1s)))]))

; EDITOR-TEXT
; Lo1s -> Image
; renders a list of 1Strings as a text image

(check-expect
 (editor-text (cons "p" (cons "o" (cons "s" (cons "t" '())))))
 (text "post" FONT-SIZE FONT-COLOR))
(check-expect
 (editor-text (cons "p" (cons "r" (cons "e" '()))))
 (text "pre" FONT-SIZE FONT-COLOR))

;(define (editor-text s)
;  (cond
;    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
;    [else
;     (text
;      (my-implode s)
;      FONT-SIZE FONT-COLOR)]))


; EDITOR-RENDER
; Editor -> Image
; renders an editor as an image of the
; two texts separated by the cursor

(check-expect
 (editor-render
  (make-editor (cons "e" (cons "r" (cons "p" '())))
               (cons "p" (cons "o" (cons "s" (cons "t" '()))))))
 (place-image/align
  (beside (text "pre" FONT-SIZE FONT-COLOR)
          CURSOR
          (text "post" FONT-SIZE FONT-COLOR))
  1 1
  "left" "top"
  MT))

(define (editor-render e)
  (place-image/align
   (beside 
    (editor-text (editor-pre e))
    CURSOR
    (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))
   

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "cd" "fgh") "\b")
              (create-editor "c" "fgh"))
(check-expect (editor-kh (create-editor "" "fgh") "\b")
              (create-editor "" "fgh"))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (create-editor "cdf" "gh"))
(check-expect (editor-kh (create-editor "cd" "") "right")
              (create-editor "cd" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (create-editor "c" "dfgh"))
(check-expect (editor-kh (create-editor "" "fgh") "left")
              (create-editor "" "fgh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor -> Editor
; move the cursor one to the left

(check-expect
 (editor-lft (make-editor '() '()))
 (make-editor '() '()))

(check-expect
 (editor-lft (make-editor (cons "a" '()) '()))
 (make-editor '() (cons "a" '())))

(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed ]
    [else
     (make-editor
      (reverse (rest (editor-pre ed)))
      (cons (first (editor-pre ed)) (editor-post ed)))]))
  

; Editor -> Editor
; move the cursor one to the right

(check-expect
 (editor-rgt
  (make-editor (cons "a" '()) '()))
 (make-editor (cons "a" '()) '()))
(check-expect
 (editor-rgt
  (make-editor (cons "a" '()) (cons "b" '())))
 (make-editor (cons "b" (cons "a" '())) '()))

(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
     (make-editor
      (cons
       (first (editor-post ed))
       (editor-pre ed))
      (rest (editor-post ed)))
     ]))

; Editor -> Editor
; delete a 1String from pre

(check-expect
 (editor-del
  (make-editor '() '()))
 (make-editor '() '()))

(check-expect
 (editor-del
  (make-editor (cons "a" '()) '()))
 (make-editor '() '()))

(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor
      (rest (editor-pre ed))
      (editor-post ed))]))


; Editor 1String -> Editor
; insert the 1String k between pre and post

(check-expect
 (editor-ins (make-editor '() '()) "e")
 (make-editor (cons "e" '()) '()))

(check-expect
 (editor-ins (make-editor (cons "d" '())
                          (cons "f" (cons "g" '()))) "e")
 (make-editor(cons "e" (cons "d" '()))
             (cons "f" (cons "g" '()))))


(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; [List-of 1String] N -> Editor
; produces a make editor based on the x-position from x

(check-expect (split-structural-integrate '("s" "h" "e") 400)
              (make-editor '("s" "h" "e") '("")))
(check-expect (split-structural-integrate '("s" "h" "e") 2)
              (make-editor '("") '("s" "h" "e")))
(check-expect (split-structural-integrate '("s" "h" "e") 15)
              (make-editor '("s") '( "h" "e")))

(define (split-structural-integrate ed0 x)
  (local (; [List-of 1String] [List-of 1String] Number -> Editor
          ; accumulator: a represents the prefix of ed
          ; accumulator: d represents the width of letters
          ; in ed0 but not in ed
          (define (split/a ed a d)
            (cond
              [(empty? ed) (make-editor (reverse a) (list ""))]
              [(<= d x
                   (+ d (image-width
                         (editor-text (list (first ed))))))
               (make-editor (if (empty? a) '("") (reverse a)) ed)]
              [else
               (split/a (rest ed)
                        (cons (first ed) a)
                        (+ (image-width
                            (editor-text (list (first ed)))) d))])))
    (split/a ed0 '() 0)))

; WorldState Number Number MouseEvent -> WorldState
(define (mouse ws x y e)
  (if (string=? e "button-down")
      (split-structural-integrate (append
                     (editor-pre ws)
                     (editor-post ws)) x)
      ws))

; main : String -> Editor
; launches the editor given some initial string

(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]
    [on-mouse mouse]))

(main "s")

































