;; file-list.scm -- unit tests for file-lists.
;; arch-tag: 3222a1d2-31b3-11d9-a678-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Nov 08, 2004 19:22

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Code:

(define *test-dir* ",file-list")
(define *test-file* (make-path *test-dir* ",file.test"))
(define *test-file-2* (make-path *test-dir* ",file.test.0"))
(define *test-file-3* (make-path *test-dir* ",file.test.1"))
(define *test-file-4* (make-path *test-dir* ",file.test.2"))

(define (remake-test-file f)
  (delete-file! f)
  (with-output-to-file f
    (lambda ()
      (write '(test)))))

(define (clean-up-test)
  (for-each delete-file!
            (list *test-file* *test-file-2* *test-file-3* *test-file-4*))
  (delete-directory! *test-dir*))

(clean-up-test)

(let* ((l1 (list *test-file* *test-file-2*))
       (l2 (cons *test-file-3* l1))
       (l3 (cons *test-file-4* l2))
       (l3-s (sort-list l3 string<?))
       (fp (lambda () (list *test-file-4*))))
  (testeez
   "Make file list"
   (test/equal "empty" (let ((fl (make-file-list))) (fl)) '())
   (test-define "make fl" fl (make-file-list *test-file* *test-file-2*))
   (test/equal "file list" (fl) l1)
   (test/equal "add file" (begin (add-to-file-list! fl *test-file-3*) (fl)) l2)
   (test/equal "add dup" (begin (add-to-file-list! fl *test-file-3*) (fl)) l2)
   (test/equal "add l" (begin (add-to-file-list! fl fp) (fl)) l3))

  (testeez
   "Delete file list"
   (test-define "fl" fl (apply make-file-list (cons fp l2)))
   (test/equal "check" (fl) l3)
   (test-true "create" (begin (make-directory! *test-dir*)
                              (map remake-test-file l3)
                              (and-map file? l3)))
   (test-false "delete" (begin (delete-file-list fl)
                               (and (or-map file? l3)
                                    (directory? *test-dir*)))))

  (testeez
   "Regexps/dirs"
   (test-true "create" (begin (make-directory! *test-dir*)
                              (map remake-test-file l3)
                              (and-map file? l3)))
   (test/equal "dir" (let ((fl (make-file-list)))
                       (add-to-file-list/dir! fl *test-dir*)
                       (sort-list (fl) string<?))
               l3-s)
   (test/equal "regexp" (let ((fl (make-file-list)))
                          (add-to-file-list/dir! fl
                                                 *test-dir*
                                                 ",file.*")
                          (sort-list (fl) string<?))
               l3-s)))

(clean-up-test)

;;; file-list.scm ends here
