;;; tests.scm --- Test directory

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


((systems spells)
 (files
  "record-types.scm"
  ("algebraic-types.scm"
   (spells algebraic-types))
  "args-fold.scm"
  "pathname.scm"
  "filesys.scm"
  ("string-utils.scm" (spells string-utils))
  ("format.scm" (spells format))
  ("misc.scm" (spells misc) (spells testing-utils) (rnrs lists))
  ("match.scm" (spells match))
  ;; these are temporarily disabled; need to port to trc-testing
  #;("opt-args.scm" (spells opt-args))
  #;("table.scm" (spells table))
  ("delimited-readers.scm"
   (spells delimited-readers)
   (spells misc)
   (srfi :8 receive)
   (srfi :13 strings)
   (rnrs io ports))
  ("operations.scm" (spells operations) (rnrs lists))
  ("ports.scm" (spells ports) (srfi :8 receive) (rnrs io ports))
  ("logging.scm")
  "process.scm"
  ("sysutils.scm"
   (spells sysutils)
   (spells pathname)
   (only (srfi :1 lists) count)
   (rnrs lists))
  ("foreign.scm"
   (spells foreign)
   (spells misc)
   (spells filesys)
   (spells gc)
   (rnrs control))
  "awk.scm"
  ("delimited-control.scm"
   (spells delimited-control))
  ("zipper-tree.scm"
   (spells zipper-tree))
  ("xvector.scm"
   (spells xvector)
   (rnrs control))
  )
)
