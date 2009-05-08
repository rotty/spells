;;; lazy-streams.sls --- Lazy streams

;; Originally written by Taylor R. Campbell and placed in the Public
;; Domain. R6RS adaption by Andreas Rottmann. 

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells lazy-streams)
  (export stream-cons
          stream-nil
          stream-null?
          stream-pair?
          stream-car
          stream-cdr
          stream->list
          list->stream
          string->stream
          stream-difference)
  (import (rnrs base)
          (spells lazy))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a d)
     (delay (cons a d)))))

(define stream-nil (delay '()))

(define-syntax define-stream-unop
  (syntax-rules ()
    ((define-stream-unop stream-op op)
     (define (stream-op stream) (op (force stream))))))

(define-stream-unop stream-null?        null?)
(define-stream-unop stream-pair?        pair?)
(define-stream-unop stream-car          car)
(define-stream-unop stream-cdr          cdr)

(define (stream->list stream)
  (let ((datum (force stream)))
    (if (pair? datum)
        (cons (car datum)
              (stream->list (cdr datum)))
        datum)))

(define (list->stream list)
  (lazy (if (pair? list)
            (stream-cons (car list)
                         (list->stream (cdr list)))
            (eager list))))

(define (string->stream string)
  (let recur ((index 0))
    (lazy (if (= index (string-length string))
              stream-nil
              (stream-cons (string-ref string index)
                           (recur (+ index 1)))))))

;** Be careful!  This operation is potentially dangerous.

(define (stream-difference earlier later)
  (lazy (if (eq? earlier later)
            stream-nil
            (stream-cons (stream-car earlier)
                         (stream-difference (stream-cdr earlier)
                                            later)))))
)
