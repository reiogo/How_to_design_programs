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
(define HAPPINESS-BAR-WIDTH
  (- (image-width HAPPINESS-BAR-OUTLINE) 2))


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
       HAPPINESS-BAR-WIDTH
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ HAPPINESS-BAR-WIDTH 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)
 )
(define (render ws)
  (cond
    [(< ws HAPPINESS-BAR-WIDTH)
     (place-image
      (rectangle
       ; start with 2 pixel in the beginning
       (max 0 (+ ws 2))
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ ws 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)]
    ; if at the end then stop growing past it
    [else
     (place-image
      (rectangle
       HAPPINESS-BAR-WIDTH
       HAPPINESS-BAR-HEIGHT
       "solid"
       "red")
      (+ (/ HAPPINESS-BAR-WIDTH 2) 2)
      Y-BAR
      HAPPINESS-BAR-OUTLINE)]
    ))

;WorldState -> WorldState
;reduces happiness by 0.1 every second
;should work based on percentages
(check-expect (tock 5) (- 5 (/ HAPPINESS-BAR-WIDTH 100)))
(check-expect (tock -4) -2)
(define (tock ws)
  ; decrease by 1 % of happiness bar 
  (max -2 (- ws (/ HAPPINESS-BAR-WIDTH 100))))

;WorldState String -> WorldState
;Increase 1/3 with up arrow, increase 1/5 with down arrow
(check-expect (key 3 "down") (+ (* (/ HAPPINESS-BAR-WIDTH 100) 20) 3))
(check-expect (key 3 "up") (+ (* (/ HAPPINESS-BAR-WIDTH 100) 33.33) 3))
(check-expect (key (+ HAPPINESS-BAR-WIDTH 2) "down")
              HAPPINESS-BAR-WIDTH)
(check-expect (key (+ HAPPINESS-BAR-WIDTH 2) "up")
               HAPPINESS-BAR-WIDTH )
(define (key ws str)
  (cond
    [(string=? str "down")
     (min HAPPINESS-BAR-WIDTH
          (+ (* (/ HAPPINESS-BAR-WIDTH 100) 20) ws))
     ]
    [(string=? str "up")
          (min HAPPINESS-BAR-WIDTH
               (+ (* (/ HAPPINESS-BAR-WIDTH 100) 33.33) ws))
     ]))

;WorldState -> WorldState
;Render a healthbar based on some initial state
(define (gauge-prog ws)
  (big-bang ws
    [on-tick tock]
    [on-draw render]
    [on-key key]
    ))
(gauge-prog 100)

