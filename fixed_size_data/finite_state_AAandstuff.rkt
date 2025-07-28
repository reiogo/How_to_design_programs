;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname finite_state_AAandstuff) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; constants
(define HEIGHT 100)
(define WIDTH 100)

(define INI
  (rectangle WIDTH HEIGHT "solid" "white"))
(define MID
  (rectangle WIDTH HEIGHT "solid" "yellow"))
(define END
  (rectangle WIDTH HEIGHT "solid" "green"))
(define ERR
  (rectangle WIDTH HEIGHT "solid" "red"))

; WorldState is one of:
; - AA
; - BC
; - DD
; - ER

; WS -> Image;
; display a white rectangle on init,
; a yellow rect on intermediate
; a green rect on end
; a red rect on error
(check-expect (render "AA") INI)
(check-expect (render "BC") MID)
(check-expect (render "DD") END)
(check-expect (render "ER") ERR)
(define (render ws)
  (cond [(string=? ws "AA") INI]
        [(string=? ws "BC") MID]
        [(string=? ws "DD") END]
        [(string=? ws "ER") ERR]))

  ;WS KeyEvent -> WS;
  ; Transition from one WS to another according to a key input
  ;KeyEvent is one of:
  ; - a
  ; - b or c
  ; - d
  ; - else
(check-expect (key "AA" "a") "BC")
(check-expect (key "BC" "b") "BC")
(check-expect (key "BC" "c") "BC")
(check-expect (key "BC" "d") "DD")
(check-expect (key "AA" "c") "ER")
(check-expect (key "BC" "a") "ER")
  (define (key ws ke)
    (cond
      [(and
        (string=? ws "AA")
        (string=? ke "a")) "BC"]
      [(and
        (string=? ws "BC")
        (or 
         (string=? ke "b")
         (string=? ke "c"))) "BC"]
      [(and
        (string=? ws "BC")
        (string=? ke "d")) "DD"]
      [else "ER"]))


; WS -> WS
; play the fsm from a certain state
(define (main ws)
  (big-bang ws
    [on-draw render]
    [on-key key]))
(main "AA")








    