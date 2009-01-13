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

          make-log-handler

          default-log-formatter

          configure-logger)
  (import (except (rnrs base) error)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs io simple)
          (xitomatl srfi lists)
          (spells time-lib)
          (spells error)
          (spells opt-args)
          (spells record-types)
          (spells include))
  
  (include-file ((spells scheme) logging)))
