;;; find-file.sls --- Search a file in a list of locations.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells find-file)
  (export find-file
          library-search-paths)
  (import (rnrs base)
          (spells filesys)
          (spells pathname)
          (spells find-file compat))

  (define (find-file filename paths)
    (let loop ((paths paths))
      (if (null? paths)
          #f
          (let ((full-name (pathname-join (pathname-as-directory (car paths)) filename)))
            (if (file-exists? full-name) full-name (loop (cdr paths))))))))
