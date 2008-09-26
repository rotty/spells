;; util.file.scm -- Gauche specific code for file.scm
;; arch-tag: d78fc229-31e1-11d9-a678-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Tue Nov 09, 2004 00:56

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

(define file-basename
  (case-lambda
    ((f) (sys-basename f))
    ((f ext) (let ((bn (sys-basename f)))
               (if (string-suffix? ext bn)
                   (string-drop-right bn (string-length ext))
                   bn)))))

(define file-dirname sys-dirname)
(define (file? f)
  (and (file-exists? f)
       (not (file-is-directory? f))))
(define (file-is-readable? f) (sys-access f R_OK))
(define (file-is-executable? f) (sys-access f X_OK))
(define (file-modification-time f) (file-mtime f))
(define directory? (with-module file.util file-is-directory?))
(define current-directory current-directory)
(define (list-dirent f) (directory-list f :children? #t))
(define (make-directory! f) (or (file-exists? f) (sys-mkdir f #o0755)))
(define make-directory* make-directory*)
(define (delete-directory! f) (if (directory? f) (sys-rmdir f)))
(define (delete-file! f) (if (file? f) (sys-unlink f)))
(define rename-file! move-file)
(define copy-file! copy-file)

;;; util.file.scm ends here
