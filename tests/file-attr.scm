;; file-attr.scm -- file.scm unit tests (file attributes)
;; arch-tag: f19f7786-303f-11d9-8bf0-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Nov 06, 2004 23:05

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
(define (remake-test-file f)
  (if (file? f) (delete-file! f))
  (with-output-to-file f
    (lambda ()
      (write "util/file.scm test"))))

(make-directory! *test-dir*)
(remake-test-file *test-file*)

(testeez
 "File attributes"
 (test-true "file? 1" (file? *test-file*))
 (test-false "file? 2" (file? *test-dir*))
 (test-false "file? 3" (file? "../abracadabra"))

 (test-true "directory? 1" (directory? "."))
 (test-true "directory? 2" (directory? ".."))
 (test-true "directory? 3" (directory? *test-dir*))
 (test-false "directory? 4" (directory? *test-file*))

 (test-true "file-is-readable? 1" (file-is-readable? "."))
 (test-true "file-is-readable? 2" (file-is-readable? ".."))
 (test-true "file-is-readable? 3" (file-is-readable? *test-dir*))
 (test-true "file-is-readable? 4" (file-is-readable? *test-file*))
 (test-false "file-is-readable? 5" (file-is-readable? "..."))

 ;;(test-true "file-is-executable? 1" (file-is-executable? "run-tests"))
 (test-true "file-is-executable? 2" (file-is-executable? "."))
 (test-true "file-is-executable? 3" (file-is-executable? *test-dir*))
 (test-false "file-is-executable? 4" (file-is-executable? *test-file*))

 (test-true "file-modification-time"
            (let ((t (file-modification-time *test-file*)))
              (sleep-seconds 1)
              (remake-test-file *test-file-2*)
              (> (file-modification-time *test-file-2*) t)))
 (test-true "file-modification-time<"
            (file-modification-time< *test-file* *test-file-2*))
 (test-true "file-modification-time>"
            (file-modification-time> *test-file-2* *test-file*)))



(delete-file! *test-file*)
(delete-file! *test-file-2*)
(delete-directory! *test-dir*)


;;; file-attr.scm ends here
