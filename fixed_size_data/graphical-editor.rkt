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

; Editor KeyEvent -> Editor
; edit the text, and manipulate the cursor
; KeyEvent is a String:
;  1String (add single character to the end of pre)
;  backspace ("\b"; delete last character of pre if exists)
;  "left" (move cursor once to left, if exists)
;  "right" (move cursor once to right, if exists)
;  "\t" (tab; ignore)
;  "\r" (return; ignore)
;  everything else (ignore)

; regular
(check-expect (edit (make-editor "hello " " world") "a")
              (make-editor "hello a" " world"))
(check-expect (edit (make-editor "hello" "") "b")
              (make-editor "hellob" ""))
(check-expect (edit (make-editor "" " world") "c")
              (make-editor "c" " world"))
; backspace
(check-expect (edit (make-editor "hello " " world") "\b")
              (make-editor "hello" " world"))
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
    [(= 1 (string-length ke))
     (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]
    [(and (string=? ke "\b")
          (> (string-length (editor-pre ed)) 0))
     (make-editor (substring
                   (editor-pre ed)
                   0
                   (- (string-length (editor-pre ed)) 2))
                  (editor-post ed))]
    [(and (string=? ke "right")
          (> (string-length (editor-post ed)) 0))
     (make-editor (editor-pre ed) (editor-post ed))]
    [(and (string=? ke "left")
          (> (string-length (editor-pre ed)) 0))
     (make-editor (editor-pre ed) (editor-post ed))]
    [else (make-editor (editor-pre ed) (editor-post ed))]))








  