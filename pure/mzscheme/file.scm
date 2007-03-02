;; system.file.scm -- Mzcheme specific code for file.scm
;; arch-tag: 8ac88eff-6220-4f9f-bab9-6ae8e51763fe

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Tue Nov 30, 2004 23:20

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

(#%require (prefix lib: (lib "list.ss"))
           (all-except (lib "file.ss")
                       find-files normalize-path)
           (rename mzscheme mzscheme-current-directory current-directory)
           (only mzscheme path->string path? absolute-path?))

(define (string-split str chr)
  (string-tokenize str (char-set-complement (char-set chr))))

(define *pset* (char-set #\/))

(define (simple-basename f)
  (cond
   ((dot-or-dotdot? f) f)
   ((string-suffix? "/.." f) "..")
   ((string-suffix? "/." f) ".")
   (else (path->string (file-name-from-path (string-trim-right f *pset*))))))

(define file-basename
  (case-lambda
    ((f) (simple-basename f))
    ((f ext) (let ((bn (simple-basename f)))
               (if (string-suffix? ext bn)
                   (string-drop-right bn (string-length ext))
                   bn)))))

(define (file-dirname f)
  (call-with-values
      (lambda () (split-path f))
    (lambda (base ign ign1)
      (cond
       ((path? base)
        (let ((base (path->string base)))
          (if (string=? base "/") "/"
              (string-trim-right base *pset*))))
       ((eq? base 'relative) ".")
       (else "/")))))

(define (file-test-p f p)
  (and (or (file? f) (directory? f))
       (if (memq p (file-or-directory-permissions f)) #t #f)))

(define (file-is-readable? f) (file-test-p f 'read))
(define (file-is-executable? f) (file-test-p f 'execute))
(define file? file-exists?)
(define directory? directory-exists?)
(define (list-dirent f) (map path->string (directory-list f)))
(define (delete-file! f) (if (file? f) (delete-file f)))
(define (delete-directory! f) (if (directory? f) (delete-directory f)))
(define rename-file! rename-file-or-directory)
(define file-modification-time  file-or-directory-modify-seconds)
(define (make-directory! f) (or (directory? f) (make-directory f)))
(define copy-file! copy-file)
(define current-directory
  (case-lambda
    (() (path->string (mzscheme-current-directory)))
    ((f) (mzscheme-current-directory f))))

(scmxlate-ignore-define make-directory* absolute-path?)

;;; system.file.scm ends here
