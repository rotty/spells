(define test-dir (x->pathname '((",filesys-test.tmp"))))

(define (create-test-file pathname)
  (call-with-output-file (x->namestring (pathname-join test-dir pathname))
    values))

(define (delete-test-file pathname)
  (delete-file (pathname-join test-dir pathname)))

(testeez "directory creation"
  (test/eqv "working state clear" (file-exists? test-dir) #f)
  (test-eval "creating stage " (begin
                                 (create-directory test-dir)
                                 (for-each create-test-file '("a" "b" "c" "foo.scm"))))
  (test/equal "fold"
    (directory-fold test-dir cons '())
    (append (map (lambda (x)
                   (pathname-with-file test-dir x))
                 '("a" "b" "c"))
            (list (pathname-with-file test-dir (make-file "foo" '("scm")))))
    ((lambda (x y)
       (lset= pathname=? x y))))
  (test-eval "deleting stage" (begin
                                (for-each delete-test-file '("a" "b" "c" "foo.scm"))
                                (delete-file test-dir))))
