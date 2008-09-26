;; util.named-args.scm -- Defining procedures with named arguments
;; arch-tag: 9c6e0292-2479-11d9-97bd-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Oct 23, 2004 00:27


;;; Comentary:

;;; Code:

(scmxlate-ignore-define args->alist)
(scmxlate-ignore-define %let-optionals*)

(define-macro (define/named-args . formals)
  (let* ((argdefs (car formals))
         (body (cdr formals))
         (name (car argdefs))
         (args (cdr argdefs))
         (arg-names (map car args))
         (arg-defs (map cadr args))
         (rbody (cond ((and (list? (car body)) (list (caar body))) body)
                      ((null? (cdr body)) (list (cons (car body) arg-names)))
                      (else (error "Invalid body")))))
    `(define-macro (,name . aa)
       (let ((aalist (map (lambda (name default)
                            (cons name
                                  (cond ((null? aa) (list default))
                                        ((assq name aa) => cdr)
                                        (else (list default)))))
                          ',arg-names
                          (list ,@arg-defs))))
         (append (list 'let aalist) ',rbody)))))

(define-macro (define/optional-args bindings body . body-rest)
  (let loop ((curr bindings) (reqd '()))
    (cond
      ((not (pair? curr))                       ; No optional bindings,
        `(define ,bindings ,body ,@body-rest))  ; regular define
      ((and (pair? (car curr)) (eq? 'optional (caar curr)))
        `(define* ,(append (reverse (cons #:optional reqd))
                           (cdar curr) (cdr curr))
           ,body ,@body-rest))
      (else (loop (cdr curr) (cons (car curr) reqd))))))

(define-macro (let-optionals* args opt-clauses . body)
  `(let-optional* args opt-clauses ,@body))

;;; util.named-args.scm ends here
