;;; delimited-readers.sls --- Read delimited strings.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Reading delimited strings.
(library (spells delimited-readers)
  (export read-line
	  read-paragraph
	  read-delimited
	  skip-char-set)
  (import (except (rnrs base) error string-copy string-for-each string->list)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (spells error)
          (spells opt-args)
          (spells include))

  (include-file ((spells scheme) delimited-readers))
  (include-file ((spells scheme) skip-char-set)))
