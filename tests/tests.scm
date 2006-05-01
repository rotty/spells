;; tests.scm -- list of test files
;; arch-tag: 81738281-305b-11d9-8bf0-00404513c0a4

;; Copyright (C) 2004-2006 by Free Software Foundation, Inc.

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
  ("assert.scm" spells.assert)
  ("condition.scm" spells.condition)
  ("cut.scm" spells.cut)
  ("file-attr.scm" spells.file spells.misc spells.namestring)
  ("file-list.scm" spells.file-list spells.file spells.namestring spells.misc)
  ("pathname.scm" spells.pathname)
  ("port.scm" spells.port srfi-6)
  ;;("file-traversal.scm" spells.file)
  ("file-write.scm" spells.file spells.namestring)
  ("file.scm" spells.file spells.namestring)
  ("sysutils.scm" spells.sysutils)
  ("format.scm" spells.format)
  ("misc.scm" spells.misc)
  ("opt-args.scm" spells.opt-args)
  ("parameter.scm" spells.parameter)
  ("byte-vectors.scm" spells.byte-vectors)
  ("process.scm" srfi-13 spells.process)
  ("table.scm" spells.table)
  ("blobs.scm" spells.blobs)
  ("delimited-readers.scm" spells.delimited-readers spells.misc srfi-6 srfi-13)
  ("operations.scm" spells.operations)
  ("logging.scm" spells.logging)))

;;; tests.scm ends here
