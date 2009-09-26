;;; xvector.sls --- 

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells xvector)
  (export make-xvector
          xvector?
          xvector-length
          xvector-ref
          xvector-set!
          xvector-push!
          xvector-pop!
          xvector-walk
          xvector->list
          list->xvector)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (srfi :8 receive)
          (srfi :9 records)
          (spells include)
          (only (spells error) make-error-signaller))

  (define error (make-error-signaller "(spells xvector)"))
  (define arithmetic-shift bitwise-arithmetic-shift)
  (define (extract-bit-field size position integer)
    (bitwise-bit-field integer position (+ position size)))
  (define integer-length bitwise-length)
  
  (include-file ((spells private) xvector)))
