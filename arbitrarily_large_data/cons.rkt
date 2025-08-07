;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cons) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(cons "Neptune"
      (cons "Uranus"
            (cons "Saturn"
                  (cons "Jupiter"
                        (cons "Mars"
                              (cons "Earth"
                                    (cons "Venus"
                                          (cons "Mercury"
                                                '()))))))))
(cons "bread"
      (cons "oatmeal"
            (cons "beans"
                  (cons "corn"
                        (cons "broccoli"
                              (cons "kale"
                                    (cons "nuts"
                                          '())))))))
(define hi
  (cons "red"
      (cons "orange"
            (cons "yellow"
                  (cons "green"
                        (cons "blue"
                              (cons "indigo"
                                    (cons "violet"
                                          '())))))))
)

(cons "mike" (cons "ike" (cons "ty" (cons "j" (cons "ray" '())))))

; A List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)
; interpretation represents a list of true or false
