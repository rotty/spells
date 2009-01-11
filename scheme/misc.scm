;; misc.scm -- Utilities that don't fit elsewhere

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Comentary:

;; Miscellaneous utility functions.

;;; Code:

;;@ Efficiently sort the list @1 using the comparison function
;; @2. Stability is not required.
(define (sort-list lst cmpf)
  (list-sort cmpf lst))

;;@ Returns the `unspecific' value, as normally returned by e.g. code
;; @code{(if #f #f)}.
(define (unspecific)
  (if #f #f))

;;@ Apply @1 to @2 (like map) and apply @code{and} to the
;; resulting list.
(define (and-map proc lst)
  (let loop ((lst lst) (res #t))
    (cond ((null? lst) res)
          (res (loop (cdr lst) (and res (proc (car lst)))))
          (else #f))))

;;@ Apply @1 to @2 (like map) and apply @code{or} to the
;; resulting list.
(define (or-map proc lst)
  (let loop ((lst lst) (res #f))
    (cond ((null? lst) res)
          (res res)
          (else (loop (cdr lst) (or res (proc (car lst))))))))

;;@ The identity function, returning @1.
(define (identity x) x)

;;@ Compose two procedures, yielding a procedure of the same arity
;;  as @2.
(define (compose f g) (lambda args (f (apply g args))))

(define (and=> e proc)
  (and e (proc e)))

;;; misc.scm ends here
