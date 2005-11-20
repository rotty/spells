;; define-values.scm -- Dialect specific file for Scheme48
; arch-tag: 300e0873-0237-47ec-8931-8b7db5be4259

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 31, 2005 21:59

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

;;; Code:

(define-syntax define-values
  (lambda (form r compare)
    (define (gensym i)
      (r (string->symbol (string-append "x" (number->string i)))))
    (destructure (((define-values values expr) form))
      (let ((tmp-vars (map gensym (iota (length values)))))
        `(,(r 'begin)
          ,@(map (lambda (name) `(,(r 'define) ,name (,(r 'unspecific)))) values)
          (,(r 'receive) ,tmp-vars ,expr
           ,@(map (lambda (var tmp) `(,(r 'set!) ,var ,tmp)) values tmp-vars)))))))

;;; define-values.scm ends here
