;;; define-values.sls --- define-values syntax.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ @code{define-value} syntax.
(library (spells define-values)
  (export define-values)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) run expand))

  ;;@args names body ...
  ;;
  ;; Evaluate @2, which should return as many values as there are
  ;; elements in @1; each identifier in @1 gets bound to the repective
  ;; value.
  (define-syntax define-values
    (lambda (form)
      (syntax-case form ()
        ((_ () exp0 exp ...)
         (with-syntax (((dummy) (generate-temporaries (list 'dummy))))
           (syntax
            (define dummy (begin exp0 exp ... 'dummy)))))
	((_ (id ...) exp0 exp ...)
	 ;; Mutable-ids are needed so that ids defined by
	 ;; define-values can be exported from a library (mutated
	 ;; variables cannot be exported).  This fix is due to Andre
	 ;; van Tonder.
	 (with-syntax (((mutable-id ...) (generate-temporaries (syntax (id ...))))
		       ((result ...)     (generate-temporaries (syntax (id ...)))))
	   (syntax
	    (begin
	      (define mutable-id) ...
	      (define dummy
		(call-with-values
                    (lambda () exp0 exp ...)
		  (lambda (result ...)
		    (set! mutable-id result) ...)))
	      (define id mutable-id) ...))))))))
