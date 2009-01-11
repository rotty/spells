;;; alist.scm --- Association list utilities.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


;;@ Return the alist @3 extended by @code{(cons @1 @2)}.
(define (acons key val alist)
  (cons (cons key val) alist))

;;@ Return the @code{cdr} of the entry in the alist @1 referred to by
;;@2 or @code{#f} if no such entry exists.
(define (assq-ref alist key)
  (cond ((assq key alist) => cdr) (else #f)))
(define (assv-ref alist key)
  (cond ((assv key alist) => cdr) (else #f)))
(define (assoc-ref alist key)
  (cond ((assoc key alist) => cdr) (else #f)))

;; arch-tag: dc503358-cbbf-4ae9-ac34-557ac99c49c4
