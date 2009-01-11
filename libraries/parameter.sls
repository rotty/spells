;;; parameter.sls --- Parameter objects.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ @uref{http://srfi.schemers.org/srfi-39/srfi-39.html, SRFI 39} -
;;  Parameter objects.
(library (spells parameter)
  (export make-parameter
          parameterize)
  (import (rnrs base)
          (rnrs lists)
          (rnrs mutable-pairs)
          (spells include))

  (include-file ((spells scheme) parameter)))
