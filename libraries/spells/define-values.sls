#!r6rs

(library (spells define-values)
  (export define-values)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) run expand))
  
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
