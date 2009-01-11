;;; condition.scm --- Additional condition types and utilities.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Comentary:

;;
;; These are common condition types which can be shared among
;; libraries (instead of each re-defining them).

;;; Code:


(define-condition-type &parser-error &error
  make-parser-error parser-error?
  (port parser-error-port))

(define-condition-type &stacked &condition
  make-stacked-condition stacked-condition?
  (next next-condition))


;;; condition.scm ends here

