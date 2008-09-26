;; file-write.scm -- tests for write operations in file.scm
;; arch-tag: 84e9a9e9-3046-11d9-8bf0-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Nov 06, 2004 23:52

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

(define *test-dir* ",file")
(define *test-file* (make-path *test-dir* ",file.test"))
(define *test-file-2* (make-path *test-dir* ",file.test.0"))
(define *test-file-3* (make-path *test-dir* ",file.test.1"))

(define (remake-test-file f)
  (delete-file! f)
  (with-output-to-file f
    (lambda ()
      (write '(test)))))

(define (clean-up-test)
  (for-each delete-file! (list *test-file* *test-file-2* *test-file-3*))
  (delete-directory! *test-dir*))


(testeez
 "File writing operations"
 (test-define "Test path" f (make-path *test-dir* "test"))
 (test-true "make-1" (begin
                       (make-directory! *test-dir*)
                       (directory? *test-dir*)))
 (test-true "make-2" (begin
                       ;; do nothing if target already exists
                       (make-directory! *test-dir*)
                       (directory? *test-dir*)))
 (test-false "del-1" (begin
                       (delete-directory! *test-dir*)
                       (directory? *test-dir*)))
 (test-false "del-2" (begin
                       (delete-directory! *test-dir*)
                       (directory? *test-dir*)))
 (test-true "make*" (begin
                      (make-directory* f)
                      (and (directory? *test-dir*)
                           (directory? f))))
 (test-false "del-3" (begin
                       (delete-directory! f)
                       (directory? f)))

 (test/equal "copy-file!" (begin
                            (remake-test-file *test-file*)
                            (copy-file! *test-file* *test-file-2*)
                            (with-input-from-file *test-file-2* read))
             '(test))

 (test/equal "rename-file!-1" (begin
                                (rename-file! *test-file-2* *test-file-3*)
                                (with-input-from-file *test-file-3* read))
             '(test))
 (test-false "rename-file!-2" (file? *test-file-2*))
 (test-true "rename-file!=3" (begin
                               (rename-file! *test-file-3* *test-file-2*)
                               (file? *test-file-2*)))
 (test/equal "rename-file!-4" (with-input-from-file *test-file-2* read) '(test))

 (test-false "delete-file!-1" (begin
                                (copy-file! *test-file* *test-file-3*)
                                (delete-file! *test-file-3*)
                                (file? *test-file-3*)))
 (test-false "delete-file!-2" (begin
                                (delete-file! *test-file-3*)
                                (file? *test-file-3*))))


(clean-up-test)


;;; file-write.scm ends here
