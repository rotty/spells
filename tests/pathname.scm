;;; pathname.scm --- unit tests for (spells pathname)

;; Copyright (C) 2004-2009 Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Fri Nov 05, 2004 12:05

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define-syntax test-pn=
  (syntax-rules ()
    ((test-pn= expected actual)
     (test-compare pathname=? expected actual))))

(define-test-suite pathname-tests
  "Spells pathname library")

(define-test-case pathname-tests comparison ()
  (test-pn= (make-pathname #f '() #f) (make-pathname #f '() #f))
  (test-compare (compose not pathname=?)
                (make-pathname #f '() #f)
                (make-pathname #f '() "foo")))

(define-test-suite (pathname-tests.s-expr pathname-tests)
  "S-Expression parsing")

(define-test-case pathname-tests.s-expr string ()
  (test-pn= (x->pathname "foo")
    (make-pathname #f '() "foo")))

(define-test-case pathname-tests.s-expr dirlist-w/o-filename ()
  (test-pn= (x->pathname '(("foo" "bar")))
    (make-pathname #f '("foo" "bar") #f)))

(define-test-case pathname-tests.s-expr dirlist-nonempty ()
  (test-pn= (x->pathname '(("foo" "bar") "baz"))
    (make-pathname #f '("foo" "bar") "baz")))

(define-test-case pathname-tests.s-expr dirlist-empty ()
  (test-pn= (x->pathname '(() "baz"))
   (make-pathname #f '() "baz")))

(define-test-suite (pathname-tests.ops pathname-tests)
  "Pathname operations")

(define-test-case pathname-tests.ops as-directory ()
  (test-pn= (x->pathname '(("foo")))
    (pathname-as-directory '(("foo"))))
  (test-pn= (x->pathname '(("foo" "bar" "baz")))
    (pathname-as-directory '(("foo" "bar") "baz"))))

(define-test-case pathname-tests.ops join ()
  (test-pn= (x->pathname '(("foo" "bar") "baz"))
    (pathname-join '(("foo" "bar") "file") '(() "baz")))
  (test-pn= (x->pathname '(("foo" "bar" "baz") "file2"))
    (pathname-join '(("foo" "bar") "file1") '(("baz") "file2")))
  (test-pn= (x->pathname '(("foo" "bar" "baz" "f2" "qux") "f3"))
    (pathname-join '(("foo" "bar") "f1")
                   '(("baz") "f2")
                   '(("qux") "f3"))))

;; The rest of the tests presume unix namestrings
(define-test-suite (pathname-tests.conversion pathname-tests)
  "Namestring conversion")

(define-test-case pathname-tests.conversion relative ()
  (test-equal (x->namestring (make-pathname #f '("foo" "bar") "baz"))
    "foo/bar/baz"))

(define-test-case pathname-tests.conversion absolute ()
  (test-equal (x->namestring (make-pathname '/ '("foo") "bar"))
    "/foo/bar"))

(define-test-case pathname-tests.conversion empty ()
  (test-equal (x->namestring (make-pathname #f '() #f))
    "."))

(define-test-case pathname-tests.conversion dir ()
  (test-equal (x->namestring (make-pathname #f '("foo") #f))
    "foo/"))


(define-test-suite (pathname-tests.type-parsing pathname-tests)
  "Filename type parsing")

(define-test-case pathname-tests.type-parsing one-type ()
  (test-pn= (x->pathname "foo.scm")
    (make-pathname #f '() (make-file "foo" "scm"))))

(run-test-suite pathname-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-pn= 1))
;; End:
