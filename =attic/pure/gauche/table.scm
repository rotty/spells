;; table.scm -- Hashtables in Gauche
;; arch-tag: 0AC484C0-D248-4EAB-8C4B-EFEA1F5B3337

;; Copyright (C) 2005, 2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega <jao@gnu.org>
;; Start date: Sat Jun 04, 2005 11:18

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(define (make-table . maybe-type)
  (make-hash-table (case (*optional maybe-type 'eq)
                     ((eq) 'eq?)
                     ((eqv) 'eqv?)
                     ((equal) 'equal?)
                     (else (error "unsupported hash type")))))

(define table? hash-table?)
(define table-ref
  (let ((magic-value '(magic)))
    (case-lambda
      ((table key) (table-ref table key default-failure-thunk))
      ((table key failure-thunk)
       (let ((value (hash-table-get table key magic-value)))
         (if (eq? value magic-value) (failure-thunk) value))))))
(define table-set! hash-table-put!)
(define table-walk hash-table-for-each)


;;; table.scm ends here
