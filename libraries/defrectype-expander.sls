;;; defrectype-expander.sls --- expanders for define-record-type*.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells defrectype-expander)
  (export expand-define-record-type*
          expand-define-functional-fields)
  (import (rnrs base)
          (xitomatl srfi receive)
          (spells lists)
          (spells parameter)
          (spells include))

  (define syntax-error error)
  
  (include-file ((spells scheme) defrectype-expander)))
