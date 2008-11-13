;;; string-substitute.sls --- String substitution

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides simple template string substitution. This library can be
;; used (maybe together with a formatter combinator library) as a
;; replacement for CL-style `format' as provided by (spells format),
;; for example.

;;; Code:

(library (spells string-substitute)
  (export string-substitute)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (spells strings)
          (spells char-set))

  (define (string-substitute template vals)
    (define (lose msg . irritants)
      (apply error 'string-substitute msg irritants))
    (let loop ((i 0) (open-pos #f) (parts '()))
      (define (handle-close-brace/escaped pos)
        (unless (doubled-char? template pos)
          (lose "unexpected close brace" template pos))
        (loop (+ pos 2) open-pos (cons (substring/shared template i (+ pos 1))
                                       parts)))
      (define (handle-open-brace pos)
        (cond ((doubled-char? template pos)
               (loop (+ pos 2) #f (cons (substring/shared template i (+ pos 1)) parts)))
              (else
               (loop (+ pos 1)
                     pos
                     (cons (substring/shared template i pos) parts)))))
      (if (not i)
          (string-concatenate-reverse parts)
          (cond (open-pos
                 (let ((close-pos (string-index template #\} i)))
                   (unless close-pos
                     (lose "unmatched opening brace" template open-pos))
                   (cond ((doubled-char? template close-pos)
                          (loop (+ close-pos 2) open-pos parts))
                         (else
                          (loop (+ close-pos 1)
                                #f
                                (cons (subst-one template open-pos close-pos vals)
                                      parts))))))
                (else
                 (let ((open-pos (string-index template #\{ i))
                       (close-pos (string-index template #\} i)))
                   (cond
                    ((not (or open-pos close-pos))
                     (loop #f #f (cons (substring/shared template i) parts)))
                    ((not open-pos)
                     (handle-close-brace/escaped close-pos))
                    ((not close-pos)
                     (handle-open-brace open-pos))
                    ((< open-pos close-pos)
                     (handle-open-brace open-pos))
                    (else
                     (handle-close-brace/escaped close-pos)))))))))

  (define (doubled-char? s i)
    (let ((c (string-ref s i)))
      (and (< (+ i 1) (string-length s))
           (char=? c (string-ref s (+ i 1))))))

  (define (subst-one template open-pos close-pos vals)
    (let ((placeholder (substring/shared template (+ open-pos 1) close-pos)))
      (vector-ref vals (string->number placeholder)))))
