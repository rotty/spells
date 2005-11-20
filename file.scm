;; file-utils.in.scm -- POSIX file access
;; arch-tag: 5ce244ab-265f-11d9-a4e7-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Mon Oct 25, 2004 10:25

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

;;; Comentary:

;; Interface for accessing POSIX file systems.

;;; Code:

;;; Path and filenames handling:

;;@ Return the base name of the file name @1, after removing
;; any trailing slash. The base name is the file name without any
;; directory components. If @2 is provided, and is equal to the end
;; of @1, it is removed also.
;; @example
;;   (basename "/tmp/test.scm" ".scm")
;;   => "test"
;; @end example
(define (file-basename f . suffix) (proc-to-be-defined))

;;@ Return the directory name component of the file name @1. If
;; @1 does not contain a directory component, @file{.} is returned.
(define (file-dirname f) (proc-to-be-defined))

;;@ Predicates testing file attributes.
(define (file-is-readable? f) (proc-to-be-defined))
(define (file-is-executable? f) (proc-to-be-defined))
(define (directory? f) (proc-to-be-defined))
(define (file? f) (proc-to-be-defined))

;;@ Return an integer representing the last file modification time.
(define (file-modification-time f) (proc-to-be-defined))

;;; Rename and delete files:

;;@ Delete given file, if it exists. Otherwise, do nothing
(define (delete-file! f) (proc-to-be-defined))

;;@ Copy file @1 to @2.
(define (copy-file! from to) (proc-to-be-defined))

;;@ Move file @1 to @2.
(define (rename-file! from to) (proc-to-be-defined))

;;; Directories:

;;@ Without arguments, get the current working directory; if @1
;; is provided, change current directory to the given one.
(define (current-directory . dir) (proc-to-be-defined))

;;@ Create a directory, provided it doesn't exist but its parent
;; does. If @1 already exists, do nothing.
(define (make-directory! f) (proc-to-be-defined))

;;@ Delete given file, if it exists and it is empty. If it doesn't
;; exist, do nothing.
(define (delete-directory! dir) (proc-to-be-defined))

;;; Directory traversal

;;@ Return a list of strings enumerating the entries in the given
;; directory. The '.' and '..' entries are @strong{not} included in the
;; returned list.
(define (list-dirent dir) (proc-to-be-defined))

;;; file-utils.in.scm ends here
