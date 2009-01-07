;; cells.sls -- Implementation of the cells datatype in terms of records

;; Copyright (C) 2007, 2008, 2009 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Wed Aug 29 15:35:22 CEST 2007

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

;;; Commentary:

;; Heavyweight (but portable) implementation of cells.

;;; Code:
#!r6rs

;;@ Mutable cells.
(library (spells cells)
  (export make-cell cell? cell-ref cell-set!)
  (import (rnrs base)
          (spells record-types))

  ;;@defun make-cell value
  ;;  Create a cell containing @var{value}.
  ;;@end defun

  ;;@defun cell? thing
  ;;  Return @code{#t} if @var{thing} is a cell.
  ;;@end defun

  ;;@defun cell-ref cell
  ;; Return the contents of @var{cell}.
  ;;@end defun

  ;;@defun cell-set! cell value
  ;; Set the contents of @var{cell} to @var{value}.
  ;;@end defun

  (define-record-type cell
    (make-cell value)
    cell?
    (value cell-ref cell-set!)))

;;; cells.sls ends here
