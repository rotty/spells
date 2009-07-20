;;; filesys.scm --- Unit tests for the filesystem interface

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;; Helper functions

(define test-dir (x->pathname '((",filesys-test.tmp"))))
(define test-data '(foo bar baz 42))

(define (create-test-file pathname)
  (call-with-output-file (x->namestring (test-file pathname))
    (lambda (port)
      (write test-data port))))

(define (delete-test-file pathname)
  (delete-file (test-file pathname)))

(define (test-file pathname)
  (pathname-join test-dir pathname))

(define (pathname-set=? s1 s2)
  (lset= pathname=? s1 s2))

(define (assert-clear-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear"  test-dir)))

(define (with-i/o-condition-symbols thunk)
  (guard (c ((i/o-file-does-not-exist-error? c) 'file-does-not-exist))
    (thunk)))


;; Tests

(define-test-suite filesys-tests
  "Filesystem interface")

(define-test-case filesys-tests file-ops
  ((description "File operations")
   (setup
    (assert-clear-stage)
    (create-directory test-dir)
    (for-each create-test-file '("a" "b" "c" "foo.scm")))
   (teardown
    (for-each delete-test-file '("a" "b" "c" "foo.scm" "out-file"))
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
        (copy-file (test-file "a") (test-file "out-file"))
        (file-size-in-bytes (test-file "out-file")))))
  (let ((outfile-name (x->namestring (test-file "out-file"))))
    (test-equal test-data
      (begin
        (call-with-output-file/atomic outfile-name
          (lambda (port)
            (write test-data port)))
        (call-with-input-file outfile-name read)))
    (test-equal (list 'exception test-data)
                (let* ((r1 (guard (c (#t c))
                             (call-with-output-file/atomic (test-file "out-file")
                               (lambda (port)
                                 (write '(some garbage) port)
                                 (raise 'exception)))))
             (r2 (call-with-input-file outfile-name read)))
        (list r1 r2)))))

(define-test-case filesys-tests delete
  ((setup
    (assert-clear-stage)
    (create-directory test-dir))
   (teardown
    (delete-file test-dir)))
  
  (test-eqv #t
    (begin
      (delete-file (test-file "not-there"))
      #t)))

(define-test-case filesys-tests file-checks
  ((setup
    (assert-clear-stage)
    (create-directory test-dir)
    (for-each create-test-file '("a")))
   (teardown
    (for-each delete-test-file '("a"))
    (delete-file test-dir)))

  (test-eqv #t (file-exists? (test-file "a")))
  (test-eqv #f (file-exists? (test-file "b")))
  (test-eqv #t (file-regular? (test-file "a")))
  (test-eqv #f (file-regular? test-dir))

  (for-each
   (lambda (pred)
     (test-eqv #f (pred (test-file "not-there"))))
   (list file-regular?
         file-directory?
         file-symbolic-link?))
  
  (for-each
   (lambda (pred)
     (test-eq 'file-does-not-exist
       (with-i/o-condition-symbols
         (lambda () (pred (test-file "not-there"))))))
   (list file-readable?
         file-writable?
         file-executable?)))

(define-test-case filesys-tests create-directory* 
  ((description "create-directory*")
   (setup
    (assert-clear-stage))
   (teardown
    (for-each delete-test-file '((("foo" "bar" "baz"))
                                 (("foo" "bar"))
                                 "foo"
                                 (())))))
  (let ((dir (test-file '(("foo" "bar" "baz")))))
    (test-eqv #f (file-exists? dir))
    (create-directory* dir)
    (test-eqv #t (file-exists? dir))
    (test-eqv #t (file-directory? dir))))

(define-test-case filesys-tests find-file ()
  (test-equal #f (find-file ".abracadabra.khgafd" (library-search-paths)))
  (test-equal #t (cond ((find-file '((spells)) (library-search-paths))
                        => file-directory?)
                       (else #f))))

(run-test-suite filesys-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
