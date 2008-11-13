;; tests.scm -- list of test files
;; arch-tag: 81738281-305b-11d9-8bf0-00404513c0a4

;; Copyright (C) 2004-2008 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Nov 07, 2004 02:22

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

((systems spells)
 (files
  ;;("assert.scm" spells.assert)
  ;;("condition.scm" spells.condition)
  ("cut.scm" spells.cut)
  ;;("file-attr.scm" spells.file spells.misc spells.namestring)
  ("pathname.scm" spells.pathname)
  ("filesys.scm" spells.filesys spells.pathname spells.lists)
  ("find-file.scm" spells.find-file)
  ;;("port.scm" spells.port srfi-6) ;; superceded by R6RS
  ;;("file-traversal.scm" spells.file)
  ;;("file-write.scm" spells.file spells.namestring)
  ;;("file.scm" spells.file spells.namestring)
  ("string-substitute.scm" spells.string-substitute)
  ("format.scm" spells.format)
  ("misc.scm" spells.misc)
  ;;("opt-args.scm" spells.opt-args) ;; temporarily disabled (can't eval definitions in R6RS)
  ("parameter.scm" spells.parameter)
  ;;("byte-vectors.scm" spells.byte-vectors) ;; superceded by R6RS
  ;;("table.scm" spells.table) ;; superceded by R6RS
  ;;("blobs.scm" spells.blobs) ;; superceded by R6RS
  ("delimited-readers.scm" spells.delimited-readers spells.misc spells.strings rnrs.io.ports)
  ("operations.scm" spells.operations)
  ("logging.scm" spells.logging)
  ("streams.scm" spells.streams)
  ("process.scm" spells.process spells.strings rnrs.io.ports)
  ("sysutils.scm" spells.sysutils spells.lists)))

;;; tests.scm ends here
