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

;; This is the generic file, which assumes that OS strings are just
;; plain strings. If the host implementation handles these
;; differently, a dialect-specific implementation must be provided.

;;; Code:
#!r6rs

(library (spells pathname os-string)
  (export os-string?
          os-string->string)
  (import (rnrs base))

  (define os-string? string?)
  (define (os-string->string os-string)
    os-string)

  )

