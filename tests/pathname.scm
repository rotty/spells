;;; pathname.scm --- unit tests for (spells pathname)

;; Copyright (C) 2004-2011 Andreas Rottmann <a.rottmann@gmx.at>
;; Start date: Fri Nov 05, 2004 12:05

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (spells misc)
        (wak trc-testing)
        (spells pathname))

(define-syntax test-pn=
  (syntax-rules ()
    ((test-pn= expected actual)
     (test-compare pathname=? expected actual))))

(define-syntax test-pns=
  (syntax-rules ()
    ((test-pns= expected actual)
     (test-compare (lambda (x y) (for-all pathname=? x y))
         expected
       actual))))


(define-test-suite pathname-tests
  "Spells pathname library")

(define-test-case pathname-tests comparison ()
  (test-pn= (make-pathname #f '() #f) (make-pathname #f '() #f))
  (test-compare (compose not pathname=?)
      (make-pathname #f '() #f)
    (make-pathname #f '() "foo"))
  (test-pn= (->pathname '(() "foo")) (->pathname '(() "foo")))
  (test-compare (compose not pathname=?)
      (->pathname '(() ("foo" "scm")))
    (->pathname '(() ("foo" "sls"))))
  (test-pns= (map ->pathname '((())
                               (() "x")
                               (() "y")
                               (("bar"))
                               (("foo"))
                               (("foo") "file1")
                               (("foo") "file2")
                               (("foo" "bar"))))
    (list-sort pathname<? (map ->pathname '((("foo" "bar"))
                                            (("foo"))
                                            (("foo") "file1")
                                            (())
                                            (() "y")
                                            (("foo") "file2")
                                            (("bar"))
                                            (() "x"))))))


(define-test-suite (pathname-tests.predicates pathname-tests)
  "Predicates")

(define-test-case pathname-tests.predicates directory-pathname? ()
  (test-eqv #t (directory-pathname? (make-pathname #f '() #f)))
  (test-eqv #f (directory-pathname? (make-pathname #f '() "foo")))
  (test-eqv #t (directory-pathname? (make-pathname #f '("foo") #f))))


(define-test-suite (pathname-tests.s-expr pathname-tests)
  "S-Expression parsing")

(define-test-case pathname-tests.s-expr string ()
  (test-pn= (->pathname "foo")
    (make-pathname #f '() "foo")))

(define-test-case pathname-tests.s-expr dirlist-w/o-filename ()
  (test-pn= (->pathname '(("foo" "bar")))
    (make-pathname #f '("foo" "bar") #f)))

(define-test-case pathname-tests.s-expr dirlist-nonempty ()
  (test-pn= (->pathname '(("foo" "bar") "baz"))
    (make-pathname #f '("foo" "bar") "baz")))

(define-test-case pathname-tests.s-expr dirlist-empty ()
  (test-pn= (->pathname '(() "baz"))
   (make-pathname #f '() "baz")))

(define-test-suite (pathname-tests.ops pathname-tests)
  "Pathname operations")

(define-test-case pathname-tests.ops as-directory ()
  (test-pn= (->pathname '(("foo")))
    (pathname-as-directory '(("foo"))))
  (test-pn= (->pathname '(("foo" "bar" "baz")))
    (pathname-as-directory '(("foo" "bar") "baz"))))

(define-test-case pathname-tests.ops merge ()
  (test-pn= (->pathname '(/ ("foo" "bar" "baz") #f))
    (merge-pathnames '(("baz")) '(/ ("foo" "bar") #f)))
  (test-pn= (make-pathname #f '("foo" "baz") "qux")
    (merge-pathnames '((back) ("baz") "qux") '(("foo" "bar"))))
  (test-pn= (->pathname '((back) ("foo" "bar") "baz"))
    (merge-pathnames '((back back back) ("foo" "bar") "baz")
                     '(("foo" "bar"))))
  (test-pn= (->pathname '(/ ("foo" "bar") "baz"))
    (merge-pathnames '((back back back) ("foo" "bar") "baz")
                     '(/ ("foo" "bar") #f))))

(define-test-case pathname-tests.ops join ()
  (test-pn= (->pathname '(("foo" "bar") "baz"))
    (pathname-join '(("foo" "bar") "file") '(() "baz")))
  (test-pn= (->pathname '(("foo" "bar" "baz") "file2"))
    (pathname-join '(("foo" "bar") "file1") '(("baz") "file2")))
  (test-pn= (->pathname '(("foo" "bar" "baz" "qux") "f3"))
    (pathname-join '(("foo" "bar") "f1")
                   '(("baz") "f2")
                   '(("qux") "f3")))
  (test-pn= (->pathname '((back back) ("some") "file2"))
    (pathname-join '((back back) ("some" "dir") "file1")
                   '((back) () "file2")))
  (test-pn= (->pathname '(/ ("another" "dir2") "file"))
    (pathname-join '(("ignore") "me")
                   '(/ ("another" "dir") #f)
                   '((back) ("dir2") "file"))))

(define-test-case pathname-tests.ops enough ()
  (test-pn= (->pathname '(("bar") "baz"))
    (enough-pathname '(("foo" "bar") "baz")
                     '(("foo"))))
  (test-pn= (make-pathname #f '() "baz")
    (enough-pathname (make-pathname '/ '("foo" "bar") "baz")
                     (make-pathname '/ '("foo" "bar") #f)))
  (test-pn= (make-pathname #f '("bar") "baz")
    (enough-pathname (make-pathname '(back back) '("bar") "baz")
                     (make-pathname '(back back) '() #f)))
  (test-pn= (make-pathname '(back) '() "bar")
    (enough-pathname (make-pathname '(back back) '() "bar")
                     (make-pathname '(back) '() #f)))
  (test-pn= (make-pathname '/ '("foo") "bar")
    (enough-pathname (make-pathname '/ '("foo") "bar")
                     (make-pathname '(back) '() #f)))
  (test-pn= (make-pathname '(back back) '("bar") "baz")
    (enough-pathname (make-pathname '(back back) '("bar") "baz")
                     (make-pathname '(back) '("bar") #f)))
  (test-pn= (make-pathname '(back) '("quz") "x")
    (enough-pathname (make-pathname '(back back) '("quz") "x")
                     (make-pathname '(back back) '("bar") #f)))
  (test-pn= (make-pathname '(back) '() "frobotz")
    (enough-pathname (make-pathname '/ '("foo" "bar") "frobotz")
                     (make-pathname '/ '("foo" "bar" "baz") "frotz")))
  (test-pn= (make-pathname '(back) '("qux") "frobotz")
    (enough-pathname (make-pathname '/ '("foo" "bar" "qux") "frobotz")
                     (make-pathname '/ '("foo" "bar" "baz") "frotz"))))

;; The rest of the tests presume unix namestrings
(define-test-suite (pathname-tests.conversion pathname-tests)
  "Namestring conversion")

(define-test-case pathname-tests.conversion relative ()
  (test-equal "foo/bar/baz"
    (->namestring (make-pathname #f '("foo" "bar") "baz"))))

(define-test-case pathname-tests.conversion absolute ()
  (test-equal "/foo/bar"
    (->namestring (make-pathname '/ '("foo") "bar"))))

(define-test-case pathname-tests.conversion empty ()
  (test-equal "."
    (->namestring (make-pathname #f '() #f))))

(define-test-case pathname-tests.conversion dir ()
  (test-equal (->namestring (make-pathname #f '("foo") #f))
    "foo/"))


(define-test-suite (pathname-tests.parsing pathname-tests)
  "Filename parsing")

(define-test-case pathname-tests.parsing types ()
  (test-pn= (make-pathname #f '() (make-file "foo" "scm"))
    (->pathname "foo.scm"))
  (test-pn= (make-pathname #f '() (make-file "foo" '("scm" "in")))
    (->pathname "foo.scm.in")))

(define-test-case pathname-tests.parsing absolute ()
  (test-pn= (make-pathname '/ '("foo") "bar")
    (->pathname "/foo/bar")))

(define-test-case pathname-tests.parsing dot ()
  (test-pn= (make-pathname #f '("some" "dir" "somewhere") #f)
    (->pathname "./some/dir/./somewhere/")))

(define-test-case pathname-tests.parsing dotdot ()
  (test-pn= (make-pathname #f '("some") "foo")
    (->pathname "some/dir/../foo"))
  (test-pn= (make-pathname #f '() #f)
    (->pathname "some/dir/../.."))
  (test-pn= (make-pathname '(back back) '("some" "dir") "file")
    (->pathname "ignore/this/../../../../some/dir/file")))

(define-test-suite (pathname-tests.to-ns pathname-tests)
  "Namestring serialization")

(define-test-case pathname-tests.to-ns types ()
  (test-equal "foo.scm"
    (->namestring (make-pathname #f '() (make-file "foo" "scm"))))
  (test-equal "foo.scm.in"
    (->namestring (make-pathname #f '() (make-file "foo" '("scm" "in"))))))

(define-test-case pathname-tests.to-ns dotdot ()
  (test-equal "../../some/dir/foo.scm"
    (->namestring (make-pathname '(back back)
                                  '("some" "dir")
                                  (make-file "foo" "scm")))))

(run-test-suite pathname-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (test-pn= 1) (test-pns= 1))
;; End:
