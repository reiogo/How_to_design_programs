;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname happiness_gauge) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;World program of happiness gauge
;design recipe
;properties
;physical
(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD (* WIDTH-OF-WORLD 0.3))
;graphical
(define HAPPINESS-BAR-OUTLINE
  (rectangle
   (* WIDTH-OF-WORLD 0.9)
   (* HEIGHT-OF-WORLD 0.5)          
   "solid"
   "black"))
(define HAPPINESS-BAR-HEIGHT
  (- (image-height HAPPINESS-BAR-OUTLINE) 3))
(define Y-BAR (+ (/ HAPPINESS-BAR-HEIGHT 2) 2))


;data representation
;WorldState is a number
;interpretation number of pixels from the right of the bar-outline
; to the end of the filled bar.

;wishes
;render, on-tick-handler, on-keys-handler,
;happiness-bar-creater
;

;WorldState -> Image
;Place inner bar with a width according
;to the given value on the outline
(check-expect
 (render 3)
 (place-image
      (rectangle
       (+ 3 2)
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ 3 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)
 )
(check-expect (render 200)
    (place-image
      (rectangle
       (- (image-width HAPPINESS-BAR-OUTLINE) 2)
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ (- (image-width HAPPINESS-BAR-OUTLINE) 2) 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)
 )
(define (render ws)
  (cond
    [(< ws (- (image-width HAPPINESS-BAR-OUTLINE) 2))
     (place-image
      (rectangle
       (+ ws 2)
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ ws 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)]
    [else
     (place-image
      (rectangle
       (- (image-width HAPPINESS-BAR-OUTLINE) 2)
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ (- (image-width HAPPINESS-BAR-OUTLINE) 2) 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)]
    ))

;WorldState -> WorldState
;reduces happiness by 0.1 every second
(define (tock ws)
  ws -)

;WorldState String -> WorldState
;Increase 1/3 with up arrow, increase 1/5 with down arrow
(define (key ws str)
  ws)


