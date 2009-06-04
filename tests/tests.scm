;;; tests.scm --- Test directory

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


((systems spells)
 (files
  ;;("assert.scm" spells.assert)
  ;;("condition.scm" spells.condition)
  ("cut.scm" spells.cut)
  ("lazy.scm"
   (spells lazy)
   (rnrs io ports))
  
  ;;("file-attr.scm" spells.file spells.misc spells.namestring)
  ("pathname.scm" spells.pathname spells.misc)
  ("filesys.scm"
   spells.filesys spells.pathname
   (except (srfi :1 lists) for-each map)
   rnrs.control rnrs.exceptions)
  ("string-utils.scm" spells.string-utils)
  ("format.scm" spells.format)
  ("misc.scm" spells.misc rnrs.lists)
  ("match.scm" spells.match)
  ;;("opt-args.scm" spells.opt-args) ;; temporarily disabled (can't eval definitions in R6RS)
  ;;("table.scm" spells.table) ;; superceded by R6RS
  ;;("blobs.scm" spells.blobs) ;; superceded by R6RS
  ("delimited-readers.scm"
   spells.delimited-readers
   spells.misc
   srfi.:8.receive
   srfi.:13.strings
   rnrs.io.ports)
  ("operations.scm" spells.operations rnrs.lists)
  ("ports.scm" spells.ports srfi.:8.receive rnrs.io.ports)
  ("logging.scm" spells.logging rnrs.lists)
  ("process.scm" spells.process srfi.:8.receive srfi.:13.strings rnrs.io.ports)
  ("sysutils.scm"
   spells.sysutils
   (only (srfi :1 lists) count)
   rnrs.lists)
  ("foreign.scm" spells.foreign spells.misc spells.filesys)))
