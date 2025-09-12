;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname database-homeversion) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; database home version\

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
; (I2) its ith Cell satisfies the ith Predicate in sch

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

(define presence-schema
  `(("Present" ,boolean?)
    ("Description" ,string?)))

(define presence-content
  '((#true "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))

(define projected-content
  `(("Alice" #true)
    ("Bob" #false)
    ("Carol" #true)
    ("Dave" #false)))
           
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

(check-expect (reorder school-db '("Age" "Present" "Name"))
              school-db-reordered)
              

(define (reorder db lol)
  (local((define old-labels (db-schema db))
         ; Content -> Content
         (define (reorder-content c)
           (map (lambda (row) (reorder-row row lol)) c))

         ; Row -> Row
         (define (reorder-row r l)
           (cond
             [(empty? l) '()]
             [else 
              (cons
               (list-ref r (index-of old-labels (first l)))
               (reorder-row r (rest l)))]))
         ; [List-of Label] Label -> Number
         (define (index lol l)
           (cond
             [(empty? lol) #f]
             [else 
              (local ((define res (index (rest lol) l)))
                (if (string=? (first lol) l)
                    0 (if (zero? res) (add1 res) res))
                ))]
)

)
(make-db
 lol
 (reorder-content (db-content db)))))



















