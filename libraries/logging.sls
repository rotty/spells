;;; logging.sls --- Logging library.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells logging)
  (export make-log

          log-entry?
          log-entry-level
          log-entry-level-name
          log-entry-object

          default-log-formatter

          configure-logger)
  (import (rnrs base)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs io simple)
          (except (srfi :1 lists) map for-each)
          (spells alist)
          (spells time-lib)
          (spells match)
          (spells opt-args)
          (spells record-types)
          (spells include))
  
  (include-file ((spells scheme) logging)))
