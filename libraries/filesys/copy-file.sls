;;; copy-file.sls --- Copy a file.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Note that this implementation does not (yet) preserve file
;; permissions (as there's no spells API to access them).

;;; Code:
#!r6rs

(library (spells filesys copy-file)
  (export copy-file)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs io ports)
          (spells ports)
          (spells pathname))

  (define x->f x->namestring)

  (define (copy-file src-pathname dst-pathname)
    (call-with-port (open-file-input-port (x->f src-pathname))
      (lambda (in-port)
        (call-with-port (open-file-output-port (x->f dst-pathname))
          (lambda (out-port)
            (copy-port in-port out-port))))))

)
