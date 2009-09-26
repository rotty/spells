;;; -*- Mode: Scheme -*-

;;;; Expanding Vectors
;;;; Test Suite

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define (iota n)
  (let loop ((i n) (list '()))
    (if (zero? i)
        list
        (let ((i* (- i 1)))
          (loop i* (cons i* list))))))


(define-test-suite xvector-tests
  "Expanding vectors")

(define-test-case xvector-tests push-pop-once ()
  (let ((xv (make-xvector)))
    (test-eqv 0 (xvector-length xv))
    (xvector-push! xv 'foo)
    (test-eqv 1 (xvector-length xv))
    (test-eqv 'foo (xvector-pop! xv))
    (test-eqv 0 (xvector-length xv))))

(define-test-case xvector-tests push-pop-list ()
  (let ((list (iota 1000))
        (xv (make-xvector)))
    (let loop ((list list))
      (if (pair? list)
          (begin
            (xvector-push! xv (car list))
            (loop (cdr list)))))
    (test-eqv (length list) (xvector-length xv))
    (for-each (lambda (index)
                (test-eqv index (xvector-ref xv index)))
              list)
    (let loop ((list* '()))
      (if (zero? (xvector-length xv))
          (test-equal list list*)
          (loop (cons (xvector-pop! xv) list*))))))

(define-test-case xvector-tests xvector-walk ()
  (let ((xvector (make-xvector)))
    (do ((i 0 (+ i 1)))
        ((= i 10000))
      (xvector-push! xvector i))
    (do ((i 0 (+ i 1)))
        ((= i 5000))
      (test-eqv (- 9999 i) (xvector-pop! xvector)))
    (let ((i 0))
      (xvector-walk xvector
        (lambda (index i*)
          (test-eqv i index)
          (test-eqv i i*)
          (set! i (+ i 1)))))))

(define-test-case xvector-tests xvector->list ()
  (let ((xvector (make-xvector)))
    (do ((i 0 (+ i 1)))
        ((= i 10000))
      (xvector-push! xvector i))
    (do ((i 0 (+ i 1)))
        ((= i 5000))
      (test-eqv (- 9999 i) (xvector-pop! xvector)))
    (test-equal (iota 5000)
      (xvector->list xvector))))

(define-test-case xvector-tests list->xvector ()
  (let ((xvector (list->xvector (iota 10000))))
    (let loop ((i 0))
      (cond ((< i 10000)
             (test-eqv i (xvector-ref xvector i))
             (loop (+ i 1)))))))

(run-test-suite xvector-tests)
