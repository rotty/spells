;; lazy.sls -- SRFI 45 (Primitives for Expressing Iterative Lazy Algorithms)
;;
;; Code taken from the SRFI's reference implementation and adapted to
;; implement 'promise?' -- Andreas Rottmann 2008
;;
;; Copyright (C) André van Tonder (2003). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

#!r6rs

;;@ Implementation of
;;  @uref{http://srfi.schemers.org/srfi-45/srfi-45.html, SRFI 45}
;;  (Primitives for Expressing Iterative Lazy Algorithms).
(library (spells lazy)
  (export delay
          lazy
          force
          eager
          promise?)
  (import (rnrs base)
          (rnrs mutable-pairs)
          (rnrs records syntactic))

  (define-record-type promise
    (fields (mutable val)))
  
  (define-syntax lazy
    (syntax-rules ()
      ((lazy exp)
       (make-promise (cons 'lazy (lambda () exp))))))

  (define (eager x)
    (make-promise (cons 'eager x)))

  (define-syntax delay
    (syntax-rules ()
      ((delay exp) (lazy (eager exp)))))

  (define (force promise)
    (let ((content (promise-val promise)))
      (case (car content)
        ((eager) (cdr content))
        ((lazy)  (let* ((promise* ((cdr content)))        
                        (content  (promise-val promise))) ; * 
                   (if (not (eqv? (car content) 'eager)) ; *
                       (begin (set-car! content
                                        (car (promise-val promise*)))
                              (set-cdr! content
                                        (cdr (promise-val promise*)))
                              (promise-val-set! promise* content)))
                   (force promise)))))))
