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
; retains only those columns from db whose label occurs in label

(check-expect
 (db-content (project db0 '("Name" "Present"))) content4)
(check-expect
 (db-content (project db0 '("Name"))) content5)
 
(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
          ; Row -> Row
          ; retains only those cells whose label is in labels
          (define (row-project row)
            ...))
    (make-db (filter keep? schema) (map row-project content))))

; Row [List-of Label] -> Row
; retain those cells whose corresponding label is in labels
(define (row-filter row label)
  '())



















  