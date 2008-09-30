;; named-args.scm -- Defining procedures with named arguments
; arch-tag: 2e478444-23b6-49bc-aeb0-da0c81628803

;; Copyright (C) 2004, 2005, 2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Oct 23, 2004 00:27


;;; Comentary:

;;; Code:


;; The following code is taken from scsh, file scsh/let-opt.scm.
;;
;; Copyright (c) 2001 Olin Shivers, BSD license.
;;

;;@ Bind arguments from an argument rest-list to variables.
;;
;; Typical usage is like this:
;; @lisp
;;   (define (foo arg1 arg2 . args)
;;     (let-optionals* ((opt1 'default1) (opt2 'default2))
;;       ...))
;; @end lisp
(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
     (let ((rest arg))
       (%let-optionals* rest (opt-clause ...) body ...)))))

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
       (lambda (rest var ...)
         (%let-optionals* rest (opt-clause ...) body ...))))
    
    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
				      (values (car arg) (cdr arg))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default '())
			     (let ((var (car arg)))
			       (if test (values var (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default #f '())
			     (let ((var (car arg)))
			       (if test (values var #t (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var supplied? rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
     (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
     (if (null? arg) (begin body ...)
	 (error "Too many arguments in let-opt" arg)))))

;;; (*optional rest-arg default-exp [test-pred])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.
;;;
;;; If there is an TEST-PRED form, it is a predicate that is used to test
;;; a non-default value. If the predicate returns false, an error is raised.

(define-syntax *optional
  (syntax-rules ()
    ((*optional rest default-exp)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg)) (car maybe-arg)
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))

    ((*optional rest default-exp arg-test)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg))
	       (let ((val (car maybe-arg)))
		 (if (arg-test val) val
		     (error "Optional argument failed test"
			    'arg-test val)))
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))))

;;; opt-args.scm ends here
