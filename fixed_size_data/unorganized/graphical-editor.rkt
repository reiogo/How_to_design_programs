;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graphical-editor) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;contants
(define HEIGHT 20)
(define WIDTH 200)
(define F-SIZE 16)
(define F-COLOR "black")
(define Y-pos (/ HEIGHT 2))

; graphical
(define CURSOR
  (rectangle 1 20 "solid" "red"))
(define BG
  (empty-scene WIDTH HEIGHT))

(define-struct editor[pre post])
; An Editor is a structure:
; (make-editor String String)
; interpretation (make-editor s t) means the text in the editor is
;(string-append s t) with the cursor displayed between s and t

; String -> String
; remove last character of a string
(check-expect (string-remove-last "hi") "h")
(define (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))

; String -> 1String
; gets the last character of a string
(check-expect (string-last "hi") "i")
(define (string-last s)
  (substring s (- (string-length s) 1) (string-length s)))

; String -> String
; gets everything but the first character
(check-expect (string-rest "hello" 1) "ello")
(define (string-rest s n)
  (substring s n (string-length s)))

; String -> 1String
;gets the first character of the string
(check-expect (string-first "world") "w")
(define (string-first s)
  (substring s 0 1))


; Editor -> Image
; show the text and the placement of the cursor
(check-expect
 (render (make-editor "hi " "world"))
 (place-image
  (beside
   (text "hi " F-SIZE F-COLOR)
   CURSOR
   (text "world" F-SIZE F-COLOR))
  (/ WIDTH 2) Y-pos
  BG))

(define (render e)
  (place-image
   (beside
    (text (editor-pre e) F-SIZE F-COLOR)
    CURSOR
    (text (editor-post e) F-SIZE F-COLOR))
   (/ WIDTH 2) Y-pos
   BG))

; Editor -> Boolean
; Check if the length of the editor would exceed the given space
(check-expect (limit (make-editor "hi" "")) #false)
(check-expect (limit (make-editor
                      "++++++++++++++++++++++++++++++++++++++++++++++++++++++"
                      "")) #true)
(define (limit ed)
  (cond
    [(> (image-width
         (text
          (string-append
           (editor-pre ed)
           (editor-post ed))
          F-SIZE F-COLOR)) (* WIDTH 0.9)) #true]
    [else #false]))

; Editor KeyEvent -> Editor
; edit the text, and manipulate the cursor
; KeyEvent is a String:
;  1String (add single character to the end of pre)
;  "\b"(backspace; delete last character of pre if exists)
;  "\t" (tab; ignore)
;  "\r" (return; ignore)
;  "left" (move cursor once to left, if exists)
;  "right" (move cursor once to right, if exists)
;  everything else (ignore)

; regular
(check-expect (edit (make-editor "hello " " world") "a")
              (make-editor "hello a" " world"))
(check-expect (edit (make-editor "hello" "") "b")
              (make-editor "hellob" ""))
(check-expect (edit (make-editor "" " world") "c")
              (make-editor "c" " world"))
; backspace
(check-expect (edit (make-editor "hello " "world") "\b")
              (make-editor "hello" "world"))
; backspace no where to go
(check-expect (edit (make-editor "" " world") "\b")
              (make-editor "" " world"))
; left arrow
(check-expect (edit (make-editor "hello" " world") "left")
              (make-editor "hell" "o world"))
; left arrow no where to go
(check-expect (edit (make-editor "" " world") "left")
              (make-editor "" " world"))
; right arrow
(check-expect (edit (make-editor "hello" " world") "right")
              (make-editor "hello " "world"))
; right arrow no where to go
(check-expect (edit (make-editor "hello" "") "right")
              (make-editor "hello" ""))
; tab
(check-expect (edit (make-editor "hello" " world") "\t")
              (make-editor "hello" " world"))

; return
(check-expect (edit (make-editor "hello" " world") "\r")
              (make-editor "hello" " world"))
; string
(check-expect (edit (make-editor "hello" " world") "hi")
              (make-editor "hello" " world"))
(define (edit ed ke)
  (cond
    [(and (string=? ke "\b")
          (> (string-length (editor-pre ed)) 0))
     (make-editor (string-remove-last (editor-pre ed))
                  (editor-post ed))]
    [(and
      (and
       (and
        (and
         (= 1 (string-length ke))
         (not (limit ed)))
        (not (string=? ke "\t")))
       (not (string=? ke "\r")))
      (not (string=? ke "\b")))
     (make-editor (string-append
                   (editor-pre ed)
                   ke)
                  (editor-post ed))]
    [(and (string=? ke "right")
          (> (string-length (editor-post ed)) 0))
     (make-editor (string-append
                   (editor-pre ed)
                   (string-first (editor-post ed)))
                  (string-rest (editor-post ed ) 1))]
    [(and (string=? ke "left")
          (> (string-length (editor-pre ed)) 0))
     (make-editor (string-remove-last (editor-pre ed))
                  (string-append
                   (string-last (editor-pre ed))
                   (editor-post ed)))]
    [else (make-editor (editor-pre ed) (editor-post ed))]))

; String -> String
; Launches an interactive editor based on initial string
(define (run s)
  (big-bang (make-editor s "")
    [to-draw render]
    [on-key edit]))




;++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define-struct editor2 [text cursor])
; (make-editor2 String Number)
; Editor2 is a structure:
; interpretation, (make-editor2 t c) t is the entire text, and cursor
; is the number of characters from the left margin.
; example usage
(define e1 (make-editor2 "Hello world" 5))
; which represents "Hello| world"





; Editor -> Image
; display the text and the cursor
(check-expect (render2 (make-editor2 "Hello world" 5))
              (place-image
               (beside
                (text "Hello" F-SIZE F-COLOR)
                CURSOR
                (text " world" F-SIZE F-COLOR))
               (/ WIDTH 2) Y-pos
               BG))
(check-expect (render2 (make-editor2 "Hello world" 0))
              (place-image
               (beside
                (text "" F-SIZE F-COLOR)
                CURSOR
                (text "Hello world" F-SIZE F-COLOR))
               (/ WIDTH 2) Y-pos
               BG))
(check-expect (render2 (make-editor2 "Hello world" 11))
              (place-image
               (beside
                (text "Hello world" F-SIZE F-COLOR)
                CURSOR
                (text "" F-SIZE F-COLOR))
               (/ WIDTH 2) Y-pos
               BG))
(define (render2 ed)

  (place-image
   (beside
    (text
     (substring
      (editor2-text ed)
      0
      (editor2-cursor ed))
     F-SIZE
     F-COLOR)
    CURSOR
    (text
     (string-rest
      (editor2-text ed)
      (editor2-cursor ed)) F-SIZE F-COLOR))
   (/ WIDTH 2) Y-pos
   BG))

; edit the text, and manipulate the cursor
; KeyEvent is a String:
;  1String (add single character to the end of pre):
;    "\b"(backspace; delete last character of pre if exists)
;    "\t" (tab; ignore)
;    "\r" (return; ignore)
;  String:
;   "left" (move cursor once to left, if exists)
;   "right" (move cursor once to right, if exists)
;  everything else (ignore)

; regular
(check-expect (edit2 (make-editor2 "hello world" 5) "a")
              (make-editor2 "helloa world" 6))
(check-expect (edit2 (make-editor2 "hello" 5) "b")
              (make-editor2 "hellob" 6))
(check-expect (edit2 (make-editor2 " world" 0) "c")
              (make-editor2 "c world" 1))
; backspace
(check-expect (edit2 (make-editor2 "hello world" 5) "\b")
              (make-editor2 "hell world" 4))
; backspace no where to go
(check-expect (edit2 (make-editor2 " world" 0) "\b")
              (make-editor2 " world" 0))
; left arrow
(check-expect (edit2 (make-editor2 "hello world" 5) "left")
              (make-editor2 "hello world" 4))
; left arrow no where to go
(check-expect (edit2 (make-editor2 " world" 0) "left")
              (make-editor2 " world" 0))
; right arrow
(check-expect (edit2 (make-editor2 "hello world" 5) "right")
              (make-editor2 "hello world" 6))
; right arrow no where to go
(check-expect (edit2 (make-editor2 "hello" 5) "right")
              (make-editor2 "hello" 5))
; tab
(check-expect (edit2 (make-editor2 "hello world" 5) "\t")
              (make-editor2 "hello world" 5))
; return
(check-expect (edit2 (make-editor2 "hello world" 5) "\r")
              (make-editor2 "hello world" 5))
; string
(check-expect (edit2 (make-editor2 "hello world" 5) "hi")
              (make-editor2 "hello world" 5))
; string is too long
(check-expect (edit2 (make-editor2 "+++++++++++++++++++++++++++++++++++++"
                                   5) "a")
                     (make-editor2 "+++++++++++++++++++++++++++++++++++++"
                                   5))
(define (edit2 ed ke)
  (cond
    [(> (string-length ke) 1)
     (cond
       [(and (string=? "right" ke)
             (not
              (=
               (editor2-cursor ed)
               (string-length (editor2-text ed)))))
        (make-editor2
         (editor2-text ed)
         (+ (editor2-cursor ed) 1))]
       [(and (string=? "left" ke)
             (> (editor2-cursor ed) 0)) (make-editor2
                                         (editor2-text ed)
                                         (- (editor2-cursor ed) 1))]
       [else ed])]
    [else (cond
            [(string=? "\b" ke )
             (cond
               [(> (editor2-cursor ed) 0)
                (make-editor2
                 (string-remove
                  (editor2-text ed)
                  (editor2-cursor ed))
                 (- (editor2-cursor ed) 1))]
               [else ed])]
            [(string=? "\t" ke) ed]
            [(string=? "\r" ke) ed]
            [(> (image-width
                 (text (editor2-text ed)
                  F-SIZE F-COLOR)) (* WIDTH 0.9)) ed]
            [else (make-editor2
                   (string-insert (editor2-text ed)
                                  (editor2-cursor ed)
                                  ke)
                   (+ (editor2-cursor ed) 1))])]))

; String Number -> String
; Remove a letter at any point in the string
(check-expect (string-remove "Hello" 2)
              "Hllo")
(define (string-remove s n)
  (string-append
   (string-remove-last
    (substring s 0 n))
   (string-rest s n)))

; String Number String -> String
; Insert a letter at any point in the string
(check-expect (string-insert "hello" 3 "e") "helelo")
(define (string-insert str i new)
  (string-append
   (substring str 0 i)
   new
   (string-rest str i)))


; String  -> String
; Launch oneline graphical editor
(define (run2 str)
  (big-bang (make-editor2 str (string-length str))
    [check-with editor2?]
    [on-draw render2]
    [on-key edit2]))

(run2 "hi")

