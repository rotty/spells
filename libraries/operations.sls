;;; operations.sls --- A simple object system, as seen in T.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ A simple object system.
(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (spells lists)
          (spells procedure-annotations)
          (spells include))
  
  (include-file ((spells scheme) operations)))
