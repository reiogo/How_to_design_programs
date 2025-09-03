;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directory_definitions) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Directories

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a Symbol

(define e0
  '((part1 part2 part3) read! ((hang draw) (read!))))

(cons
 (cons 'part3 (cons 'part2 (cons 'part1 '())))
 (cons
  'read!
  (cons
   (cons (cons 'hang (cons 'draw '())) (cons (cons 'read! '()) '()))
   '()
   )))

; Dir.v1 -> Number
; Count how many files in a given directory

(check-expect (how-many e0) 7)
(check-expect (how-many '()) 0)

(define (how-many d)
  (cond
    [(empty? d) 0]
    [(symbol? (first d))
     (add1 (how-many (rest d)))]
    [else
     (+
      (how-many (first d))
      (how-many (rest d)))]))

(define-struct dir [name content])
;(define-struct dir [name size readability content])
; A Dir.v2 is a structure:
; (make-dir Symbol LOFD)

; A LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a Symbol

(define
  e1 (make-dir
      'TS
      (list
       (make-dir
        'Text
        '(part1 part2 part3))
       'read!
       (make-dir
        'Libs
        (list
         (make-dir 'Code '(hange draw))
         (make-dir 'Docs '(read!)))))))

; Dir.v2 -> Number
; Count the number of files in a given directory

(check-expect (how-many.v2 e1) 7)
(check-expect (how-many.v2 (make-dir 'none '())) 0)


(define (how-many-a.v2 dir)
  (local (; LOFD -> Number
          ; Count number of files in lofd
          (define (how-many-lofd lofd)
            (cond
              [(empty? lofd) 0]
              [(symbol? (first lofd))
               (add1 (how-many-lofd (rest lofd)))]
              [else
               (+
                (how-many.v2 (first lofd))
                (how-many-lofd (rest lofd)))])))
    
    (how-many-lofd (dir-content dir))))

(define (how-many.v2 dir)
  (foldr
   (lambda (lofd rst)
     (if (symbol? lofd) (add1 rst)
         (+ (how-many.v2 lofd) rst)))
   0
   (dir-content dir)))

(define-struct file [name size content])

; A File.v3 is a structure:
; (make-file Symbol N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure:
; (make-dir.v3 Symbol Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; (cons File.v3 File*)

(define e2
  (make-dir.v3
   'TS
   (list
    (make-dir.v3
     'Text
     '()
     (list
      (make-file 'part1 99 "")
      (make-file 'part2 52 "")
      (make-file 'part3 17 "")))
    (make-dir.v3
     'Libs
     (list
      (make-dir.v3
       'Code
       '()
       (list
        (make-file 'hang 8 "")
        (make-file 'draw 2 "")))
      (make-dir.v3
       'Docs
       '()
       (list (make-file 'read! 19 ""))))
     '()))
   (list (make-file 'read! 10 ""))))

; Dir.v3 -> Number
; count how many files in a dir

(check-expect (how-many.v3 e2) 7)
(check-expect (how-many.v3 (make-dir.v3 'none '() '())) 0)

(define (how-many.v3  dir)
  (+
   (foldr (lambda (dirs rst) (+ (how-many.v3 dirs) rst)) 0 (dir.v3-dirs dir))
   (foldr (lambda (f rst) (add1 rst)) 0 (dir.v3-files dir))))


(define (atom? x)
  (cond
    [(or
      (number? x)
      (string? x)
      (symbol? x)) #t]
    [else #f]))


(check-expect (find? e2 'hang) #t)

(check-expect 

(define (find? dir s)
  (local
    (
     (define (find-dirs? dirs)
       (cond
         [(empty? dirs) #f]
         [else
          (or
           (find? (first dirs) s)
           (find-dirs? (rest dirs)))]))
     (define (find-files? f)
       (cond
         [(empty? f) #f]
         [else
          (or
           (symbol=? (file-name (first f)) s)
           (find-files? (rest f)))]))
     )
    (or
     (find-dirs? (dir.v3-dirs dir))
     (find-files? (dir.v3-files dir)))))






















