;; util.misc.scm -- Misc utilites for s48
;; arch-tag: 1e0b6c72-492f-406a-8758-7473ff7bc946

;; Copyright (C) 2005, 2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Jun 12, 2005 22:19

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(define (sleep-seconds seconds)
  (sleep (* seconds 1000)))

(define (two-byte-vector-ref bytev k)
  (bitwise-ior (arithmetic-shift (byte-vector-ref bytev k) 8)
               (byte-vector-ref bytev (+ k 1))))

;; procedure-arity initially by Taylor Campbell,
;; http://paste.lisp.org/display/9042
(define (procedure-arity proc)
  (let* ((code (template-ref (closure-template proc) 0))
         (protocol (byte-vector-ref code 1)))
    (cond ((<= protocol maximum-stack-args)
           protocol)
          ((= protocol two-byte-nargs-protocol)
           (two-byte-vector-ref code 2))
          ((= protocol two-byte-nargs+list-protocol)
           `(>= ,(two-byte-vector-ref code 2)))
          ((= protocol args+nargs-protocol)
           `(>= ,(byte-vector-ref code 2)))
          ((= protocol nary-dispatch-protocol)
           ;; There's no way to know exactly what the procedure expects
           ;; reasonably.
           #f)
          (else
           (error "I don't know how to disassemble this protocol."
                  protocol)))))

(define (thunk? p)
  (and (procedure? p)
       (let ((arity (procedure-arity p)))
         (or (equal? arity 0)
             (and (pair? arity) (= (cadr arity) 0))))))

(define (scheme-dialect) 'scheme48)

;;; util.misc.scm ends here
