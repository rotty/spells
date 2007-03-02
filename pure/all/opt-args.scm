;; named-args.scm -- Defining procedures with named arguments

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Oct 23, 2004 00:27

;; auxiliar syntax
(define-syntax args->alist
  (syntax-rules ()
    ((_ ((label? value?) ...)) (list (cons 'label? value?) ...))
    ((_ x? ...) (error "unexpected arguments format" 'x? ...))))


;;@ macro making it easy to define functions that take named
;; arguments of the form @code{(name (label value) (label2 value2) ...)}
;;
;; it can be used as:
;; @example
;;    (define/named-args (name (label default) ...) <forms or procedure>)
;; @end example
(define-syntax define/named-args
  (syntax-rules ()
    ((_ (name? (arg? def?) ...) (fun? aa? ...))
     (define/named-args
       (name? (arg? def?) ...)
       (lambda (arg? ...) (fun? aa? ...)) "define/named-args"))
    ((_ (name? (arg? def?) ...) proc?)
     (define/named-args
       (name? (arg? def?) ...) proc? "define/named-args"))
    ((_ (name? (arg? def?) ...) proc? "define/named-args")
     (define-syntax name?
       (syntax-rules ()
         ((_ . aa?)
          (let ((aalist (args->alist aa?)))
            (let ((arg? (cond ((null? aalist) def?)
                              ((assq 'arg? aalist) => cdr)
                              (else def?)))...)
              (proc? arg? ...)))))))
    ((_ (name? (arg? def?) ...) form1? form2? rest? ...)
     (define/named-args (name? (arg? def?) ...)
       (lambda (arg? ...) form1? form2? rest? ...) "define/named-args"))))

(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals arg ...)
     (let-optionals* arg ...))))

;;@ macro that allows for simple definition of functions with
;; optional arguments.
;;
;; it can be used as:
;; @example
;;   (define/optional-args (name (arg ... (optional (optarg default) ...)))
;;     expr ...)
;; @end example
(define-syntax define/optional-args
  (syntax-rules (optional)
    ((_ (name . bindings) . bodies)
      (define/optional-args "seek-optional" bindings () ((name . bindings) . bodies)))

    ((_ "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
        (let-optionals* _rest _opt-bindings
                        . _bodies)))

    ((_ "seek-optional" (x . rest) (reqd ...) form)
     (define/optional-args "seek-optional" rest (reqd ... x) form))
    
    ((_ "seek-optional" not-a-pair reqd form)
     (define . form))                 ; no optional found, regular define
    
    ((_ name body)                    ; just the definition for 'name',
     (define name body))              ; for compatibilibility with define
    ))

; Copyright (c) 1993-2005 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Library functionality for writing procedures with variable number of arguments.

; This has the same interface as the OPT-LAMBDA in PLT Scheme's etc.ss
; library.

;;@args clause...
;;
;; Macro that creates a procedure with default arguments. Each clause
;; can either be an identifier, which names a variable like in regular
;; @code{lambda}. It can also be of the form @code{(name value)},
;; which creates an optional argument @code{name} with the default
;; value @code{value}.
(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda (?clause1 . ?clauses) ?body1 ?body ...)
     (opt-lambda-aux-1 (?clause1 . ?clauses) () ?body1 ?body ...))
    ((opt-lambda ?id ?body1 ?body ...)
     (lambda ?id ?body1 ?body ...))))

; process the initial vanilla parameters
(define-syntax opt-lambda-aux-1
  (syntax-rules ()
    ((opt-lambda-aux-1 () (?arg ...) ?body ...)
     (lambda (?arg ...) ?body ...))
    ((opt-lambda-aux-1 ((?id ?default) . ?rest) (?arg ...) ?body ...)
     (opt-lambda-aux-2 ((?id ?default) . ?rest)
		       (?arg ... . rest) rest ()
		       ?body ...))
    ((opt-lambda-aux-1 (?id . ?rest) (?arg ...) ?body ...)
     (opt-lambda-aux-1 ?rest (?arg ... ?id) ?body ...))))

; this processes from the optionals on
(define-syntax opt-lambda-aux-2
  (syntax-rules ()
     ((opt-lambda-aux-2 () ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause ...)
	 ?body ...)))
    ;; optimization
    ((opt-lambda-aux-2 ((?id ?default))
		       ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause
	      ...
	      (?id (if (pair? ?rest-param)
		       (car ?rest-param)
		       ?default)))
	 ?body ...)))
    ((opt-lambda-aux-2 ((?id ?default) ?rest ...)
		       ?args ?rest-param (?lclause ...) ?body ...)
     (opt-lambda-aux-2 (?rest ...)
		       ?args
		       new-rest
		       (?lclause ...
				 (?id (if (pair? ?rest-param)
					  (car ?rest-param)
					  ?default))
				 (new-rest (if (pair? ?rest-param)
					       (cdr ?rest-param)
					       '())))
		       ?body ...))
    ;; kludge for dealing with rest parameter
    ((opt-lambda-aux-2 ((?id ?default) . (?rest1 . ?rest))
		       ?args ?rest-param (?lclause ...) ?body ...)
     (opt-lambda-aux-2 (?rest1 . ?rest)
		       ?args
		       new-rest
		       (?lclause ...
				 (?id (if (pair? ?rest-param)
					  (car ?rest-param)
					  ?default))
				 (new-rest (if (pair? ?rest-param)
					       (cdr ?rest-param)
					       '())))
		       ?body ...))
    ((opt-lambda-aux-2 ((?id ?default) . ?rest)
		       ?args ?rest-param (?lclause ...) ?body ...)
     (lambda ?args
       (let* (?lclause
	      ...
	      (?id (if (pair? ?rest-param)
		       (car ?rest-param)
		       ?default))
	      (?rest (if (pair? ?rest-param)
			 (cdr ?rest-param)
			 '())))
	 ?body ...)))))

