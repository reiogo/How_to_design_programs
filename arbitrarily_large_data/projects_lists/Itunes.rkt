;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Itunes) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))

; ITUNES
(define ITUNES-LOCATION "itunes.xml")

; LTracks
;(define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))


; the 2htdp/itunes library documentation, part I:

; An LTracks is one of;
; - '()
; - (cons Track LTracks)


;(define-struct track (name artist album time track# added play# played))
; A Track is a structure:
; (create-track String String String N N Date N Date)
; interpretation An instance records in order: the track's title, its
; producing artist, to which album it belongs, its playing time in
; milliseconds, its position with the album, the date it was added,
; how often it has been played, and the date when it was last played

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track if the inputs belong to the proper classes
; otherwise it produces #false.

;(define (create-track name artist album time track# added play# played)
;  ...)


;(define-struct date (year month day hour minute second))
; A Date is a structure:
; (create-date N N N N N N)
; interpretation An instance records six pieces of information: the
; date's year, month (between 1 and 12 inclusive), day (between 1
; and 31), hour (between 0 and 23), minute (between 0 and 59), and
; second (also between 0 and 59).

; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date if the inputs belong to the proper classes
; otherwise it produces #false.

(create-date 2020 07 08 09 09 09)

(create-date 2025 08 09 09 10 12)

;(define (create-date y mo day h m s)
; ...)

; String -> LTracks
; creates a list of tracks representation for all tracks in file-name,
; which reads an XML export from an iTunes library
;(define (read-itunes-as-tracks ITUNES-LOCATION)
;...)


; EXAMPLE
(define EG
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
    (create-date 2024 08 09 09 25 12))))

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

; LTRacks -> N
; finds the total amount of time a collection of tracks have been played

(check-expect
 (total-time
  EG)
 (+ 240000 260000 190000))

(check-expect
 (total-time
  '())
 0)

(define (total-time l)
  (cond
    [(empty? l) 0]
    [else
     (+
      (track-time (first l))
      (total-time (rest l)))]))

; LTracks -> List-of-strings
; creates a list of album titles from a collection of tracks

(check-expect
 (select-all-album-titles
  EG)
 (list "Denim" "Denim" "Denim"))

(check-expect
 (select-all-album-titles
  '())
 '())

(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (track-album (first l))
      (select-all-album-titles (rest l)))]))

; List-of-strings ->List-of-strings
; filters by unique

(check-expect
 (create-set
  (list "abc" "abc" "acb"))
 (list "abc" "acb"))

(check-expect
 (create-set
  '())
 '())
 
(define (create-set alos)
  (cond
    [(empty? alos) '()]
    [else
     (if
      (in-set? (first alos) (rest alos))
      (create-set(rest alos))
      (cons (first alos) (create-set (rest alos))))]))

; String List-of-strings -> Boolean
; Check whether s is in alos

(check-expect
 (in-set? "a" (list "a" "b"))
 #true)

(check-expect
 (in-set? "a" (list "c" "b"))
 #false)

(define (in-set? s alos)
  (cond
    [(empty? alos) #false]
    [else
     (or
      (string=? s (first alos))
      (in-set? s (rest alos)))]))

; LTracks -> List-of-Strings
; A unique list of album titles from a list of tracks

(check-expect
 (select-album-titles/unique EG)
 (list "Denim"))

(check-expect
 (select-album-titles/unique '())
 '())

(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))

;(select-album-titles/unique
;itunes-tracks)

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
  (cond
    [(empty? lt) '()]
    [else
     (if (string=?
          (track-album (first lt))
          a)
         (cons
          (first lt)
          (select-album a (rest lt)))
         (select-album a (rest lt)))]))

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
  (cond
    [(empty? lt) '()]
    [else
     (if
      (and
       (compare-dates d (track-played (first lt)))
       (string=? (track-album (first lt)) al))
      (cons (first lt) (select-album-date al d (rest lt)))
      (select-album-date al d (rest lt)))]))


; Date Date -> Boolean
; Check if d1 is earlier than d2

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 09 08 01 01 02))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 02)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2026 09 08 01 01 02)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 10 08 01 01 01))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 08 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 06 01 01 01)
  (create-date 2025 09 07 01 01 01))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 09 01 01 01)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 09 08 02 01 01))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 08 02 01 01)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 09 08 01 02 01))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 08 01)
  (create-date 2025 09 08 01 01 01))
 #false)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 01)
  (create-date 2025 09 08 01 01 02))
 #true)

(check-expect
 (compare-dates
  (create-date 2025 09 08 01 01 04)
  (create-date 2025 09 08 01 01 02))
 #false)

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

; LTracks -> list-of-LTracks
; Consumes LTracks and creates a list of LTracks for each album

(check-expect
 (select-albums MIX)
 (list EG
       (list (create-track
              "Trinity"
              "Choir"
              "DUC"
              190000
              3
              (create-date 2020 07 08 09 09 09)
              400
              (create-date 2025 08 09 09 25 12)))))

(check-expect
 (select-albums '())
 '())

(define (select-albums lt)
  (cond
    [(empty? lt) '()]
    [else
     (group (select-album-titles/unique lt) lt)]))


; List-of-Strings LTracks -> List-of-LTracks
; Puts tracks from lt into a list of ltracks according to the
; album titles from alos

(check-expect
 (group
  (list "Denim" "DUC")
  MIX)
 (list EG (list (create-track
                 "Trinity"
                 "Choir"
                 "DUC"
                 190000
                 3
                 (create-date 2020 07 08 09 09 09)
                 400
                 (create-date 2025 08 09 09 25 12)))))

(check-expect
 (group '() MIX)
 '())

(define (group alos lt)
  (cond
    [(empty? alos) '()]
    [else
     (cons
      (select-album (first alos) lt)
      (group (rest alos) lt))]))


; LAssoc
(define LASSOC-EG
  (list
   (list "hi" #false)
   (list "ho" 3)
   (list "fee" "fi")
   (list "fo" (create-date 2025 09 09 09 09 09))))

(define ITUNES-EG
  (list
   (list "hi" #false)
   (list "ho" 3)
   (list "fee" 227996)
   (list "Total Time" 237996)
   (list "Total Time" 337996)
   (list "fo" (create-date 2025 09 09 09 09 09))))

; LLists
(define LL-A
  (list LASSOC-EG LASSOC-EG))

; String LAssoc Any -> Association
; uses a key and returns the first associated element
; if not found returns default

(check-expect
 (find-association
  "hi" LASSOC-EG "none")
 (list "hi" #false))

(check-expect
 (find-association
  "hive" LASSOC-EG "none")
 "none")

(define (find-association key ala default)
  (cond
    [(empty? ala) default]
    [else
     (if
      (string=? key (first (first ala)))
      (first ala)
      (find-association key (rest ala) default))]))

; LLists -> N
; produces the total amount of play time

(check-expect
 (total-time/list ITUNES-EG)
 (+ 237996 337996))

(check-expect
 (total-time/list '())
 0)
                  

(define (total-time/list all)
  (cond
    [(empty? all) 0]
    [else (if
           (string=? "Total Time" (first (first all)))
           (+ (second (first all))
              (total-time/list (rest all)))
           (total-time/list (rest all)))]))

; LLists -> List-of-strings
; groups strings that are associated with a boolean value

(check-expect
 (boolean-attributes LASSOC-EG)
 (list "hi"))

(check-expect
 (boolean-attributes '())
 '())

(check-expect
 (boolean-attributes
  (list
   (list "hi" #false)
   (list "height" #true)
   (list "ho" 3)
   (list "fee" "fi")
   (list "fo" (create-date 2025 09 09 09 09 09))))
 (list "hi" "height"))

(define (boolean-attributes all)
  (cond
    [(empty? all) '()]
    [else
     (if
      (boolean? (second (first all)))
      (cons (first (first all))
      (boolean-attributes (rest all)))
      (boolean-attributes (rest all)))]))

; LAssoc -> Track
; convert ala into a track if possible

(check-expect
 (track-as-struct
  (list
   (list "Name" "Japan")
   (list "Artist" "Daniel Caesar")
   (list "Album" "Denim")
   (list "Total Time" 190000)
   (list "Track Count" 3)
   (list "Year" (create-date 2020 07 08 09 09 09))
   (list "Play Count" 400)
   (list "Play Date" (create-date 2024 08 09 09 25 12))))
 (create-track
    "Japan"
    "Daniel Caesar"
    "Denim"
    190000
    3
    (create-date 2020 07 08 09 09 09)
    400
    (create-date 2024 08 09 09 25 12)))

(define (track-as-struct ala)
  (create-track
   (second (find-association "Name" ala "None"))
   (second (find-association "Artist" ala "None"))
   (second (find-association "Album" ala "None"))
   (second (find-association "Total Time" ala 0))
   (second (find-association "Track Count" ala 0))
   (second (find-association "Year" ala (create-date 2025 01 01 01 01 01)))
   (second (find-association "Play Count" ala 0))
   (second (find-association "Play Date" ala (create-date 2025 01 01 01 01 01)))
   ))

























