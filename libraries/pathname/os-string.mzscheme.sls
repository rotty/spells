;;; os-string.sls --- Operating-system string abstraction.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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

;; Handles OS-strings as used by MzScheme.

;;; Code:
#!r6rs

(library (spells pathname os-string)
  (export os-string?
          os-string->string)
  (import (rnrs base)
          (only (mzscheme) path? path->string))


  (define os-string? path?)
  (define (os-string->string os-string)
    (path->string os-string))
  
  )

