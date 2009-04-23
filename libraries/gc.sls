#!r6rs
(library (spells gc)
  (export unwind-protect
          make-guardian
          make-weak-cell weak-cell-ref weak-cell?
          collect)
  (import (rnrs base)
          (rnrs control)
          (spells misc)
          (spells gc compat))



;; Code and comments in this section is adapted from
;; <http://mumble.net/~campbell/scheme/unwind-protect.scm>, authored
;; by Taylor Campbell.

;;; Unwind Protection

;;; (UNWIND-PROTECT <thunk> <protector>) applies the nullary procedure
;;; <thunk> in the dynamic context of the call to UNWIND-PROTECT, and
;;; when control can no longer re-enter <thunk>, applies the nullary
;;; procedure <protector> in the dynamic context of the call to
;;; UNWIND-PROTECT.
;;;
;;; This procedure needs better a name.  The author welcomes
;;; suggestions.  See also mit-unwind-protect.scm and
;;; rewind-protect.scm.
;;;
;;; WARNING:  You may not use UNWIND-PROTECT within a rewind-protected
;;; extent, because UNWIND-PROTECT arranges for its protector to be
;;; called within the dynamic context of the call to UNWIND-PROTECT.
;;; If UNWIND-PROTECT were called within a rewind-protected extent,
;;; control may need to re-enter the rewind-protected extent to call
;;; the protector, but rewind-protected extents may not be re-entered.

;;; Assumptions:  After (REGISTER-FINALIZER <object> <procedure>), if
;;; <object>'s storage is reclaimed by the garbage collector, then at
;;; some future time <procedure> will be applied to zero arguments.
;;; (NO-OP <object>) returns an unspecified value but guarantees that
;;; the compiler will not allow <object> to be reclaimed if control
;;; ever reaches that call.

(define unwind-guardian (make-guardian))

(define (unwind-protect thunk protector)
  (do ((t (unwind-guardian) (unwind-guardian)))
      ((not t))
    ((cdr t)))
  (let ((twonkie (cons 0 (preserving-dynamic-context protector))))
    (unwind-guardian twonkie)
    (dynamic-wind
     (lambda () (identity twonkie))
     thunk
     values)))

(define (preserving-dynamic-context thunk)
  (let ((dynamic-context (current-dynamic-context)))
    (lambda ()
      (with-dynamic-context dynamic-context thunk))))

(define (current-dynamic-context)
  ((call-with-current-continuation
     (lambda (call-with-captured-dynamic-context)
       (lambda ()
         call-with-captured-dynamic-context)))))

(define (with-dynamic-context dynamic-context thunk)
  (call-with-current-continuation
    (lambda (return-to-original-context)
      (dynamic-context
       (lambda ()
         (call-with-values thunk return-to-original-context))))))

)
