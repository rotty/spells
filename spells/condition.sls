;;; condition.sls --- Extra condition types

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Additional condition types.
(library (spells condition)
  (export &parser-error make-parser-error parser-error?
          parser-error-port
          &stacked make-stacked-condition stacked-condition? next-condition

          display-condition
          
          ;; This doesn't really belong here
          limited-write)
  (import (rnrs)
          (spells include))
  
  (include-file ((spells private) condition)))
