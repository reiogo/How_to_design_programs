; Data Definitions ===========================================================

; Dir is a built-in so it's only implicitly defined.
; It's v3 since it was iteratively defined in directory_definitions.rkt

; A Dir.v3 is a structure:
; (make-dir.v3 Symbol Dir* File*)

; A File.v3 is a structure:
; (make-file Symbol N String)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; (cons File.v3 File*)


; Function Definitions ===========================================================

; String -> Dir
; creates a data representation of the directory that a-path identifies
; (define (create-dir a-path) ...)

(define d2
  (create-dir
   "/Users/evolany994/personal_repos"))


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

