;;; cut.sls --- Alias library for SRFI 26 (Notation for Specializing
;;; Parameters without Currying)

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs
(library (spells cut)
  (export cut cute)

  (import (rnrs base)
          (spells include))
  
  (include-file ((spells private) cut)))
