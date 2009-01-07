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
