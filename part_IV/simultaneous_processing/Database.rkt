;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Database) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Project Database

(define-struct db (schema content))
; A DB is a structure:
; (make-db Schema Content)

; A Schema is a [List-of Spec].

; A Spec is a [List Label Predicate].

; A Label is a String.

; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row].

; A Row is [List-of Cell].

; A Cell is Any.
; constraint cells in databases do not contain functions

; integrity constraint In (make-db sch con), for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satsifies the ith Predicate in sch

(define sch0
  `(("Name" ,string?) ("Age" ,integer?) ("Present" ,boolean?)))
(define sch1
  `(("Present" ,boolean?) ("Description" ,string?)))
(define sch2
  `(("Name" ,string?) ("Present" ,boolean?)))

(define content0
  `(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))
(define content1
  `((#true "presence") (#false "absence")))
(define content2
  `((s 35 #true)))
(define content3
  `(("alice" 35 #true 's)))
(define content4
  `(("Alice" #true)
    ("Bob" #false)
    ("Carol" #true)
    ("Dave" #false)))
(define content5
  `(("Alice")
    ("Bob")
    ("Carol")
    ("Dave")))

(define db0 (make-db sch0 content0))
(define db1 (make-db sch1 content1))
(define db2 (make-db sch0 content2))
(define db3 (make-db sch0 content3))
(define db4 (make-db sch2 content4))

(define school-schema
  `(("Name" ,string?)
    ("Age" ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob" 25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define school-schema-reordered
  `(("Age" ,integer?)
    ("Present" ,boolean?)
    ("Name" ,string?)))

(define school-content-reordered
  `(( 35 #true "Alice")
    ( 25 #false "Bob")
    ( 30 #true "Carol")
    ( 32 #false "Dave")))

(define school-db-reordered
  (make-db school-schema-reordered
           school-content-reordered))

(define school-schema-reordered.v2
  `(("Age" ,integer?)
    ("Name" ,string?)))

(define school-content-reordered.v2
  `(( 35 "Alice")
    ( 25  "Bob")
    ( 30  "Carol")
    ( 32  "Dave")))

(define school-db-reordered.v2
  (make-db school-schema-reordered.v2
           school-content-reordered.v2))

(define presence-schema
  `(("Present" ,boolean?)
    ("Description" ,string?)))

(define presence-content
  '((#true "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))

(define presence-content.v2
  '((#true "presence")
    (#true "here")
    (#false "absence")
    (#false "there")))

(define presence-db.v2
  (make-db presence-schema
           presence-content.v2))

(define join-schema
  `(("Name" ,string?)
    ("Age" ,integer?)
    ("Description" ,boolean?)))

(define join-content
  `(("Alice" 35 "presence")
    ("Bob" 25 "absence")
    ("Carol" 30 "presence")
    ("Dave" 32 "absence")))

(define join-db
  (make-db join-schema
           join-content))

(define join-content.v2
  `(("Alice" 35 "presence")
    ("Alice" 35 "here")
    ("Bob" 25 "absence")
    ("Bob" 25 "there")
    ("Carol" 30 "presence")
    ("Carol" 30 "here")
    ("Dave" 32 "absence")
    ("Dave" 32 "there")))

(define join-db.v2
  (make-db join-schema
           join-content.v2))

(define projected-content
  `(("Alice" #true)
    ("Bob" #false)
    ("Carol" #true)
    ("Dave" #false)))
           


(define-struct spec (label predicate))
; Spec is a structure: (make-spec Label Predicate)

(define sch0.v2
  (list (make-spec "Name" string?)
        (make-spec "Age" integer?)
        (make-spec"Present" boolean?)))
(define sch1.v2
  (list (make-spec "Present" boolean?)
        (make-spec "Description" string?)))
(define db0.v2 (make-db sch0.v2 content0))
(define db1.v2 (make-db sch1.v2 content1))

; DB -> Boolean
; do all rows in db live up to constraints (I1) (I2)

(check-expect (integrity-check db0) #true)
(check-expect (integrity-check db1) #true)
(check-expect (integrity-check db2) #f)
(check-expect (integrity-check db3) #f)

(define (integrity-check.v1 db)
  (local (; Row -> Boolean
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (= (length row) (length (db-schema db)))
                 (andmap (lambda (s c) [(second s) c]) (db-schema db) row))))
    (andmap row-integrity-check (db-content db))))

(define (integrity-check.v2 db)
  (local ((define schema (db-schema db))
          ; Row -> Boolean
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (= (length row) (length schema))
                 (andmap (lambda (s c) [(second s) c]) schema row))))
    (andmap row-integrity-check (db-content db))))

(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define width (length schema))
          ; Row -> Boolean
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(second s) c]) schema row))))
    (andmap row-integrity-check content)))

; [X X -> Boolean] [List-of X] [List-of X] -> Boolean
; andmap but for two lists
; assumes the lists are the same lengths

(check-expect
 (andmap2 (lambda (x f) (f x))
          '(1 "string" #true)
          (list integer? string? boolean?)) #t)
(check-expect
 (andmap2 (lambda (x f) (f x))
          '(1 's #true)
          (list integer? string? boolean?)) #f)

(define (andmap2 f l0 l1)
  (cond
    [(empty? l0) #t]
    [else
     (and
      (f (first l0) (first l1))
      (andmap2 f (rest l0) (rest l1)))]))

; DB [List-of Label] -> DB
; retain only those columns from db whose label occurs in labels

(check-expect (db-content (project school-db '("Name" "Present")))
              projected-content)

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (first c) labels))

          (define mask (map keep? schema))
          
          ; Row -> Row
          ; retains only those cells whose label is in labels
          (define (row-project row)
            (foldr (lambda (cell m rst)
                     (if m
                         (cons cell rst)
                         rst)) '() row mask))
          )
    (make-db (filter keep? schema) (map row-project content))))


;(define labels '("Name" "Present"))
;(check-expect (row-filter '("alice" 32 #true) '("Name" "Age" "Present"))
;              '("alice" #true))


; DB [List-of Label] Predicate -> [List-of Row]
; selects list of rows where the cells satisfy the given predicate
; projected down to the given label(s)

(check-expect
 (select school-db '("Name") (lambda (row)
                               (< (string-length (first row)) 5)))
 '(("Bob") ("Dave")))

(define (select db lol p)
  (filter p (db-content (project db lol))))

; DB lol -> DB
; reorder the columns

; '("Name" "Age" "Present") '("alice" 35 #true) -> '(35 #true "alice")

(check-expect
 (map (lambda (spec) (first spec))
      (db-schema (reorder school-db '("Age" "Present" "Name"))))
 (map (lambda (spec) (first spec)) (db-schema school-db-reordered)))

(check-expect
 (db-content (reorder school-db '("Age" "Present" "Name")))
 (db-content school-db-reordered))

(check-expect
 (map (lambda (spec) (first spec))
      (db-schema (reorder school-db '("Age" "Present" "Name"))))
 (map (lambda (spec) (first spec)) (db-schema school-db-reordered)))

(check-expect
 (db-content (reorder school-db '("Age" "Pres" "Name")))
 (db-content school-db-reordered.v2))
              

(define (reorder db lol)
  (local((define old-labels (db-schema db))
         ; [List-of Label] Label -> Number
         (define (index lol l)
           (foldr (lambda (current-label rst)
                    (if (string=? (first current-label) l)
                        0 (if (boolean? rst) rst (add1 rst)))
                    ) #f lol))
         
         (define new-order
           (filter (lambda (x) (if (boolean? x) #f #t))
                   (map (lambda (l) (index old-labels l)) lol)))
         
         ; Schema -> Schema
         (define (reorder-schema s)
           (filter (lambda (x) (if (boolean? x) #f #t))
                   (map (lambda (i) (if (boolean? i) i (list-ref s i))) new-order)))
     
         ; Content -> Content
         (define (reorder-content c)
           (map
            (lambda (row)
              (map (lambda (i) (if (boolean? i) i (list-ref row i))) new-order)
              ) c)))
    (make-db
     (reorder-schema old-labels)
     (reorder-content (db-content db)))))

(define (reorder.v1 db lol)
  (local((define old-labels (db-schema db))
         
         ; [List-of Label] Label -> Number
         (define (index lol l)
           (foldr (lambda (current-label rst)
                    (if (string=? (first current-label) l)
                        0 (if (boolean? rst) rst (add1 rst)))
                    ) #f lol))
         
         (define new-order (map (lambda (l) (index old-labels l)) lol))
         
         ; Schema -> Schema
         (define (reorder-schema s lol0)
           (map (lambda (i) (list-ref s i)) new-order))
     
         ; Content -> Content
         (define (reorder-content c)
           (map
            (lambda (row)
              (map (lambda (i) (list-ref row i)) new-order)
              ) c))

         )
    (make-db
     (reorder-schema old-labels lol)
     (reorder-content (db-content db)))))

(define (reorder.v-1 db lol)
  (local((define old-labels (db-schema db))
         
         ; Schema -> Schema
         (define (reorder-schema s lol0)
           (cond [(empty? lol0) '()]
                 [else
                  (cons (list-ref s (index old-labels (first lol0)))
                        (reorder-schema s (rest lol0)))]))
         
         ; Content -> Content
         (define (reorder-content c)
           (map (lambda (row) (reorder-row row lol)) c))
         
         ; Row -> Row
         (define (reorder-row r l)
           (cond [(empty? l) '()]
                 [else
                  (cons
                   (list-ref r (index old-labels (first l)))
                   (reorder-row r (rest l)))]))
         ; [List-of Label] Label -> Number
         (define (index lol l)
           (cond [(empty? lol) #f]
                 [else
                  (local ((define res (index (rest lol) l)))
                    (if (string=? (first (first lol)) l)
                        0
                        (if (boolean? res) res (add1 res))) )] )) )
    (make-db
     (reorder-schema old-labels lol)
     (reorder-content (db-content db)))))

; DB DB -> DB
; joins two db's
; last spec in the schema of db-1 must be
; the same as the first spec in the schema of db-2

(check-expect
 (map (lambda (spec) (first spec))
      (db-schema (join school-db presence-db)))
 (map (lambda (spec) (first spec)) (db-schema join-db)))

(check-expect
 (db-content (join school-db presence-db))
 (db-content join-db))

(check-expect
 (map (lambda (spec) (first spec))
      (db-schema (join school-db presence-db.v2)))
 (map (lambda (spec) (first spec)) (db-schema join-db.v2)))

(check-expect
 (db-content (join school-db presence-db.v2))
 (db-content join-db.v2))


              
(define (join.v1 db-1 db-2)
  (local (
          ; Schema -> Schema
          (define (new-schema s1 s2)
            (cond
              [(empty? (rest s1)) (rest s2)]
              [else
               (cons
                (first s1)
                (new-schema (rest s1) s2))]))

          ; Content Any -> Row
          (define (find-to-be-joined c x)
            (rest (first (filter
                          (lambda (row) (if (equal? (first row) x) #t #f))
                          c)))
            )

          ; Row -> Row
          (define (new-row r1)
            (cond
              [(empty? (rest r1))
               (find-to-be-joined (db-content db-2) (first r1))]
              [else
               (cons
                (first r1)
                (new-row (rest r1)))]))
          
          ; Content -> Content
          (define (new-content c1)
            (map (lambda (row) (new-row row)) c1))
          )
    (make-db
     (new-schema (db-schema db-1) (db-schema db-2))
     (new-content (db-content db-1)))))

(define (join.v2 db-1 db-2)
  (local (
          ; Schema -> Schema
          (define (new-schema s1 s2)
            (cond
              [(empty? (rest s1)) (rest s2)]
              [else
               (cons
                (first s1)
                (new-schema (rest s1) s2))]))

          ; Content Any -> Row
          (define (find-to-be-joined c x)
            (rest (first (filter
                          (lambda (row) (if (equal? (first row) x) #t #f))
                          c)))
            )

          ; Row -> [List-of Row]
          (define (get-rows r1)
            (cond
              [(empty? (rest r1))
                 (filter
                  (lambda (row)
                    (if (equal? (first row) (first r1)) #t #f))
                  (db-content db-2))]
              [else
                (get-rows (rest r1))]))

          ; Row Row -> Row
          (define (at-end row0 row1)
            (cond
              [(empty? (rest row0)) row1]
              [else
               (cons
                (first row0)
                (at-end (rest row0) row1))]))

          ; Row -> Row
          (define (new-row r1)
            (map (lambda (g-r) (at-end r1 (rest g-r))) (get-rows r1)))
          
          ; Content -> Content
          (define (new-content c1)
            (foldr (lambda (row rst) (append (new-row row) rst)) '() c1))
          )
    (make-db
     (new-schema (db-schema db-1) (db-schema db-2))
     (new-content (db-content db-1)))))


(define (join db-1 db-2)
  (local (
          ; Schema -> Schema
          (define (new-schema s1 s2)
            (cond
              [(empty? (rest s1)) (rest s2)]
              [else
               (cons
                (first s1)
                (new-schema (rest s1) s2))]))

          ; Row -> [List-of Row]
          (define (get-rows r1)
            (cond
              [(empty? (rest r1))
                 (filter
                  (lambda (row)
                    (if (equal? (first row) (first r1)) #t #f))
                  (db-content db-2))]
              [else
                (get-rows (rest r1))]))

          ; Row Row -> Row
          (define (at-end row0 row1)
            (cond
              [(empty? (rest row0)) row1]
              [else
               (cons
                (first row0)
                (at-end (rest row0) row1))]))

          ; Row -> Row
          (define (new-row r1)
            (map (lambda (g-r) (at-end r1 (rest g-r))) (get-rows r1)))
          
          ; Content -> Content
          (define (new-content c1)
            (foldr (lambda (row rst) (append (new-row row) rst)) '() c1))
          )
    (make-db
     (new-schema (db-schema db-1) (db-schema db-2))
     (new-content (db-content db-1)))))














