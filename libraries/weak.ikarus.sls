;;; weak.ikarus.sls --- Weak pointers for Ikarus.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(library (spells weak)
  (export make-weak-pointer weak-pointer-ref weak-pointer?
          make-guardian)
  (import (rnrs base)
          (only (ikarus) weak-cons weak-pair? bwp-object? make-guardian))

  (define (make-weak-pointer obj)
    (weak-cons obj #f))

  (define (weak-pointer? thing)
    (weak-pair? thing))

  (define (weak-pointer-ref weak-pointer)
    (let ((obj (car weak-pointer)))
      (and (not (bwp-object? obj)) obj))))
