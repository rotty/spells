;;; process.sls --- OS process interface.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Process interface.
(library (spells process)
  (export process?
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process
          close-process-ports

          call-with-process-input
          call-with-process-output
          
          run-process
          run-process/string
          run-process/lines
          run-process/sexps)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs io ports)
          (rnrs io simple)
          (srfi :8 receive)
          (srfi :1 lists)
          (srfi :13 strings)
          (spells pathname)
          (spells delimited-readers)
          (spells process compat)
          (spells include))
  
  (include-file ((spells scheme) process)))
