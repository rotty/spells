(define test-dir (x->pathname '((",filesys-test.tmp"))))

(define (create-test-file pathname)
  (call-with-output-file (x->namestring (test-file pathname))
    values))

(define (delete-test-file pathname)
  (delete-file (test-file pathname)))

(define (test-file pathname)
  (pathname-join test-dir pathname))

(define (pathname-set=? s1 s2)
  (lset= pathname=? s1 s2))

(define-test-suite filesys-tests
  "Filesystem interface")

(define-test-case filesys-tests file-ops
  ((description "File operations")
   (setup
    (when (file-exists? test-dir)
      (test-failure "working stage not clear"  test-dir))
    (create-directory test-dir)
    (for-each create-test-file '("a" "b" "c" "foo.scm")))
   (teardown
    (for-each delete-test-file '("a" "b" "c" "foo.scm" "a-copied"))
    (delete-file test-dir)))
  (begin
    (test-compare
     pathname-set=?
     (append (map (lambda (x)
                    (pathname-with-file test-dir x))
                  '("a" "b" "c"))
             (list (pathname-with-file test-dir (make-file "foo" '("scm")))))
     (directory-fold test-dir cons '()))
    (test-equal (file-size-in-bytes (test-file "a"))
      (begin
        (copy-file (test-file "a") (test-file "a-copied"))
        (file-size-in-bytes (test-file "a-copied"))))))

(define-test-case filesys-tests find-file ()
  (test-equal #f (find-file ".abracadabra.khgafd" (library-search-paths)))
  (test-equal #t (cond ((find-file '((spells)) (library-search-paths))
                        => file-directory?)
                       (else #f))))

(run-test-suite filesys-tests)
