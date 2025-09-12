;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Itunes-abstractions) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))
; Abstractions - Itunes

(define MIX
  (list
   (create-track
    "Blue Jeans"
    "Daniel Caesar"
    "Denim"
    240000
    3
    (create-date 2020 07 08 09 09 09)
    400
    (create-date 2025 08 09 09 10 12))

   (create-track
    "Danny B"
    "Daniel Caesar"
    "Denim"
    260000
    3
    (create-date 2020 07 08 09 09 09)
    400
    (create-date 2024 08 09 09 15 12))

   (create-track
    "Japan"
    "Daniel Caesar"
    "Denim"
    190000
    3
    (create-date 2020 07 08 09 09 09)
    400
    (create-date 2024 08 09 09 25 12))
   (create-track
    "Trinity"
    "Choir"
    "DUC"
    190000
    3
    (create-date 2020 07 08 09 09 09)
    400
    (create-date 2025 08 09 09 25 12))))


; SELECT-ALBUM-DATE ===========================================

; String Date LTracks -> LTracks
; produce ltracks from al, played after d

(check-expect
 (select-album-date
  "Denim"
  (create-date 2024 10 09 10 10 10)
  MIX)
 (list (create-track
        "Blue Jeans"
        "Daniel Caesar"
        "Denim"
        240000
        3
        (create-date 2020 07 08 09 09 09)
        400
        (create-date 2025 08 09 09 10 12))))

(check-expect
 (select-album-date
  "Denim"
  (create-date 2025 01 01 01 01 01)
  '())
 '())
                    
(define (select-album-date al d lt)
  (local (; Track -> Boolean
          ; Check if track is in al and after d
          (define (in-al-after-d? t)
            (and
             (string=? (track-album t) al)
             (compare-dates d (track-played t))))) 
  (filter in-al-after-d? lt)))

;  (cond
;    [(empty? lt) '()]
;    [else
;     (if
;      (and
;       (compare-dates d (track-played (first lt)))
;       (string=? (track-album (first lt)) al))
;      (cons (first lt) (select-album-date al d (rest lt)))
;      (select-album-date al d (rest lt)))]))

; COMPARE-DATES ===========================================
(define (compare-dates d1 d2)
  (cond
    [(< (date-year d1) (date-year d2)) #true]
    [(> (date-year d1) (date-year d2)) #false]
    [(< (date-month d1) (date-month d2)) #true]
    [(> (date-month d1) (date-month d2)) #false]
    [(< (date-day d1) (date-day d2)) #true]
    [(> (date-day d1) (date-day d2)) #false]
    [(< (date-hour d1) (date-hour d2)) #true]
    [(> (date-hour d1) (date-hour d2)) #false]
    [(< (date-minute d1) (date-minute d2)) #true]
    [(> (date-minute d1) (date-minute d2)) #false]
    [(< (date-second d1) (date-second d2)) #true]
    [(> (date-second d1) (date-second d2)) #false]
    [else #false]))


; SELECT-ALBUM ===========================================

;String Ltracks -> Ltracks
; get Ltracks from lt that belong to album a

(check-expect
 (select-album "DUC" MIX)
 (list (create-track
        "Trinity"
        "Choir"
        "DUC"
        190000
        3
        (create-date 2020 07 08 09 09 09)
        400
        (create-date 2025 08 09 09 25 12))))

(check-expect
 (select-album "DUC" '())
 '())

(define (select-album a lt)
  (local (; Track -> Boolean
          ; check if track is in album a
          (define (check-in-album t)
            (string=? (track-album t) a)))
  (filter check-in-album lt)))

;  (cond
;    [(empty? lt) '()]
;    [else
;     (if (string=?
;          (track-album (first lt))
;          a)
;         (cons
;          (first lt)
;          (select-album a (rest lt)))
;         (select-album a (rest lt)))]))



















