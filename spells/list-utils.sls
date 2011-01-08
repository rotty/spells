#!r6rs
;;; list-utils.sls --- List utilities

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:


;;; Code:

(library (spells list-utils)
  (export list-intersperse)
  (import (rnrs))

;; Return a new list obtained by inserting `elem' between all elements
;; of `lst'
(define (list-intersperse lst elem)
  (if (null? lst)
      lst
      (let loop ((l (cdr lst)) (result (list (car lst))))
        (if (null? l)
            (reverse result)
            (loop (cdr l) (cons (car l) (cons elem result)))))))

)
