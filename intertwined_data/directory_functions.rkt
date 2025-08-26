;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directory_functions) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp")) #f)))
; Directory functions

; String -> Dir.v3
; creates a data representation of the directory that a-path identifies
; (define (create-dir a-path) ...)

;(define d0
;  (create-dir
;   "/Users/evolany994/personal_repos/functional_programming/intertwined_data"))
;
;(define d1
;  (create-dir
;   "/Users/evolany994/dir_func"))
;(define d2
;  (create-dir
;   "/Users/evolany994/personal_repos"))

(define d0
  (create-dir
   "/home/ro/repos/drracket/intertwined_data"))
(define d1 d0)
(define d2
  (create-dir
   "/home/ro/repos/drracket"))


; A Dir.v3 is a structure:
; (make-dir.v3 Symbol Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; (cons File.v3 File*)


; Dir.v3 -> Number
; count how many files in a dir

(define (how-many.v3  dir)
  (+
   (foldr (lambda (dirs rst) (+ (how-many.v3 dirs) rst)) 0 (dir-dirs dir))
   (foldr (lambda (f rst) (add1 rst)) 0 (dir-files dir))))
;(how-many.v3 d0)


; Dir String -> Boolean
; is the given name in the dir?

(check-expect
 (find? d0 "poetry.rkt") #t)

(check-expect
 (find? d0 "poet.rkt") #f)
(check-expect
 (find? (make-dir 'hi '() (list
                           (make-file "x" 333 (make-date 2035 9 23 23 23 23) "")))
        "x") #t)

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
           (string=? (file-name (first f)) s)
           (find-files? (rest f)))]))
     )
    (or
     (find-dirs? (dir-dirs dir))
     (find-files? (dir-files dir)))))

; Dir -> [List-of String]
; list the names of all files and directories in a given dir

;(check-expect
; (ls d1) '("test" "find.txt" "test.txt"))
(check-expect
 (ls (make-dir "hi" '() '())) '())


(define (ls dir)
  (append
   (ls-dirs (dir-dirs dir))
   (ls-files (dir-files dir))))

(define (ls-a dir)
  (append
   (list (dir-name dir))
   (ls-dirs (dir-dirs dir))
   (ls-files(dir-files dir))))

(define (ls-dirs dirs)
  (foldr (lambda (dir rst)
           (append
            (ls-a dir) rst)) '() dirs))

(define (ls-files files)
  (foldr (lambda (file rst)
           (cons (file-name file) rst))
         '()
         files))

; Dir -> Number
; compute total size of all files

(check-expect (du (make-dir 'hi '() '())) 0)

(define (du dir)
  (+
   (du-dirs (dir-dirs dir))
   (du-files (dir-files dir))))

(define (du-dirs dirs)
  (cond
    [(empty? dirs) 0]
    [else
     (+ 
      (du (first dirs))
      (du-dirs (rest dirs)))]
    ))

(define (du-files files)
  (cond
    [(empty? files) 0]
    [else
     (+
      (file-size (first files))
      (du-files (rest files)))]))

; A Path is [List-of Symbol].
; interpretation directions on how to find a file in a directory tree

; Dir String -> Path
; produce the path to f in d

(check-expect (find d0 "poetry.rkt")
              '("intertwined_data" "poetry.rkt"))
(check-expect (find d2 "match.rkt")
              '("functional_programming" "intertwined_data" "poetry.rkt"))
;(check-expect
; (find d1 "find.txt")
; '("dir_func" "test" "find.txt"))

(define (find d f)
  (if
   (find? d f)
   (make-path d f)
   #false))

(define (make-path d f)
  (local
    ((define (make-path-dirs dirs)
       (cond
         [(empty? dirs) '()]
         [else
          (local ((define check (make-path (first dirs) f)))
            (if
             (empty? check)
             (make-path-dirs (rest dirs))
             check
             ))]))
     (define (make-path-files files)
       (cond
         [(empty? files) '()]
         [else
          (if
           (string=? (file-name (first files)) f)
           (list f)
           (make-path-files (rest files)))]))
     (define (last l)
       (cond
         [(empty? (rest l)) (first l)]
         [else
           (last (rest l))]))
     )
    (local ((define path (make-path-files (dir-files d))))
      (append (list (dir-name d))
              (if (empty? path)
                  (if
                   (string=? (last (make-path-dirs (dir-dirs d))) f)
                   (make-path-dirs (dir-dirs d) f)
                   '())
                  path)))))




















