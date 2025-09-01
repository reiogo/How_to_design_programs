;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname directory_functions) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp")) #f)))
; Directory functions

; String -> Dir.v3
; creates a data representation of the directory that a-path identifies
; (define (create-dir a-path) ...)

(define d0
  (create-dir
   "/Users/evolany994/personal_repos/functional_programming/intertwined_data"))

(define d1
  (create-dir
   "/Users/evolany994/dir_func"))
(define d2
  (create-dir
   "/Users/evolany994/personal_repos"))
(define d3
  (create-dir
   "/Users/evolany994/personal_repos"))

;(define d0
;  (create-dir
;   "/home/ro/repos/drracket/intertwined_data"))
;(define d1 d0)
;(define d2
;  (create-dir
;   "/home/ro/repos/drracket"))


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
              '("personal_repos" "functional_programming" "intertwined_data" "match.rkt"))
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
    ((define check-this-file
       (local ((define in-files
                 (foldl (lambda (file rst)
                          (if (string=? (file-name file) f)
                              (list f)
                              rst)) #false (dir-files d))))
         (if
          (false? in-files)
          (foldl (lambda (dir rst)
                   (local ((define in-dir (make-path dir f)))
                     (if
                      (false? in-dir)
                      rst
                      in-dir))) #false (dir-dirs d))
          in-files))))
    (if
     (false? check-this-file)
     #false
     (cons (dir-name d) check-this-file)
     )))

; Dirs String -> [List-of Path]
; finds all paths of f in d

(check-expect (find-all d2 "match.rkt")
              '(("personal_repos"
                 "functional_programming"
                 "intermezzo"
                 "match.rkt")
                ("personal_repos"
                 "functional_programming"
                 "intertwined_data"
                 "match.rkt")))

(check-expect (find-all d2 "test")
              (list
               (list "personal_repos" "codingqs" "c_programming" "test")
               (list "personal_repos" "functional_programming" "test")) )

(define (find-all d f)
  (local ((define result
            (local (
                    (define file-paths
                      (foldr (lambda (file rst)
                               (if (string=? (file-name file) f) (cons (list f) rst) rst))
                             '() (dir-files d)))
                    (define dir-paths
                      (foldr (lambda (dir rst)
                               (append (find-all dir f) rst)) '() (dir-dirs d))))
              (append
               (if (empty? file-paths) '() file-paths)
               (if (empty? dir-paths) '() dir-paths)))))
    (if (empty? result)'()
        (map (lambda (path) (cons (dir-name d) path)) result))))


; Dir -> [List-of Path]
; lists paths to all the files in a give dir

(check-expect (ls-R d1)
              '(("dir_func" "test.txt")
                ("dir_func" "test" "find.txt")
                ))

(define (ls-R dir)
  (map (lambda (path) (cons (dir-name dir) path)) 
   (append
    (ls-R-files (dir-files dir))
    (ls-R-dirs (dir-dirs dir)))))

(define (ls-R-files files)
  (cond
    [(empty? files) '()]
    [else
     (cons
      (list (file-name (first files)))
      (ls-R-files (rest files)))]))

(define (ls-R-dirs dirs)
  (cond
    [(empty? dirs) '()]
    [else
     (append
      (ls-R (first dirs))
      (ls-R-dirs (rest dirs)))]))

; Dirs String -> [List-of Path]
; finds all paths of f in d

(check-expect (find-all-r d2 "match.rkt")
              '(("personal_repos"
                 "functional_programming"
                 "intermezzo"
                 "match.rkt")
                ("personal_repos"
                 "functional_programming"
                 "intertwined_data"
                 "match.rkt")))

(check-expect (find-all-r d2 "test")
              (list
               (list "personal_repos" "codingqs" "c_programming" "test")
               (list "personal_repos" "functional_programming" "test")) )

(define (find-all-r d f)
  (local ((define (last l)
            (cond
              [(empty? (rest l)) (first l)]
              [else
                (last (rest l))])))
    (filter (lambda (path)
            (string=? (last path) f)) (ls-R d))))
























