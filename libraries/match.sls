;;; match.sls --- Pattern matching.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Pattern matching.
(library (spells match)
  (export match
          match-lambda
          match-lambda*
          match-let
          match-let*
          match-letrec)
  (import (rnrs base)
          (rnrs mutable-pairs)
          (spells define-values)
          (spells include))

  (include-file ((spells scheme) match)))
