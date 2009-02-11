;;; string-utils.sls --- String utilities

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:


;;; Code:
#!r6rs

(library (spells string-utils)
  (export string-split
          string-substitute)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (except (rnrs unicode) string-titlecase string-upcase string-downcase)
          (rnrs io simple)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (srfi :26 cut))


  ;; -- procedure+: string-split STRING
  ;; -- procedure+: string-split STRING '()
  ;; -- procedure+: string-split STRING '() MAXSPLIT
  ;;
  ;; Returns a list of whitespace delimited words in STRING.
  ;; If STRING is empty or contains only whitespace, then the empty list
  ;; is returned. Leading and trailing whitespaces are trimmed.
  ;; If MAXSPLIT is specified and positive, the resulting list will
  ;; contain at most MAXSPLIT elements, the last of which is the string
  ;; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
  ;; non-positive, the empty list is returned. "In time critical
  ;; applications it behooves you not to split into more fields than you
  ;; really need."
  ;;
  ;; -- procedure+: string-split STRING CHARSET
  ;; -- procedure+: string-split STRING CHARSET MAXSPLIT
  ;;
  ;; Returns a list of words delimited by the characters in CHARSET in
  ;; STRING. CHARSET is a list of characters that are treated as delimiters.
  ;; Leading or trailing delimeters are NOT trimmed. That is, the resulting
  ;; list will have as many initial empty string elements as there are
  ;; leading delimiters in STRING.
  ;;
  ;; If MAXSPLIT is specified and positive, the resulting list will
  ;; contain at most MAXSPLIT elements, the last of which is the string
  ;; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
  ;; non-positive, the empty list is returned. "In time critical
  ;; applications it behooves you not to split into more fields than you
  ;; really need."
  ;;
  ;; This is based on the split function in Python/Perl
  ;;
  ;; (string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
  ;; (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
  ;; (string-split " abc d e f  " '() 0) ==> ()
  ;; (string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
  ;; (string-split ":" '(#\:)) ==> ("" "")
  ;; (string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
  ;; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
  ;; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
  ;; (string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")
  ;;
  (define (string-split str . rest)
    ;; maxsplit is a positive number
    (define (split-by-whitespace str maxsplit)
      (define (skip-ws i yet-to-split-count)
        (cond
         ((>= i (string-length str)) '())
         ((char-whitespace? (string-ref str i))
          (skip-ws (+ i 1) yet-to-split-count))
         (else (scan-beg-word (+ i 1) i yet-to-split-count))))
      (define (scan-beg-word i from yet-to-split-count)
        (cond
         ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
         (else (scan-word i from yet-to-split-count))))
      (define (scan-word i from yet-to-split-count)
        (cond
         ((>= i (string-length str))
          (cons (substring str from i) '()))
         ((char-whitespace? (string-ref str i))
          (cons (substring str from i)
                (skip-ws (+ i 1) (- yet-to-split-count 1))))
         (else (scan-word (+ i 1) from yet-to-split-count))))
      (skip-ws 0 (- maxsplit 1)))

    ;; maxsplit is a positive number
    ;; str is not empty
    (define (split-by-charset str cs maxsplit)
      (define (scan-beg-word from yet-to-split-count)
        (cond
         ((>= from (string-length str)) '(""))
         ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
         (else (scan-word from from yet-to-split-count))))
      (define (scan-word i from yet-to-split-count)
        (cond
         ((>= i (string-length str))
          (cons (substring str from i) '()))
         ((char-set-contains? cs (string-ref str i))
          (cons (substring str from i)
                (scan-beg-word (+ i 1) (- yet-to-split-count 1))))
         (else (scan-word (+ i 1) from yet-to-split-count))))
      (scan-beg-word 0 (- maxsplit 1)))

    ;; resolver of overloading...
    ;; if omitted, maxsplit defaults to
    ;; (inc (string-length str))
    (if (string-null? str) '()
        (if (null? rest)
            (split-by-whitespace str (+ 1 (string-length str)))
            (let ((charset (car rest))
                  (maxsplit
                   (if (pair? (cdr rest)) (cadr rest) (+ 1 (string-length str)))))
              (cond
               ((not (positive? maxsplit)) '())
               ((null? charset) (split-by-whitespace str maxsplit))
               ((pair? charset)
                (split-by-charset str (list->char-set charset) maxsplit))
               (else
                (split-by-charset str (->char-set charset) maxsplit))))))
    )

  ;;@ Simple template string substitution.
  ;;
  ;; This library can be used (maybe together with a formatter
  ;; combinator library) as a replacement for CL-style `format' as
  ;; provided by (spells format), for example.
  (define string-substitute
    (case-lambda
      ((dst template vals grammar)
        (%string-substitute dst template vals grammar))
      ((template vals grammar)
       (string-substitute #f template vals grammar))
      ((template vals)
       (string-substitute #f template vals 'braces))))

  (define (%string-substitute dst template vals grammar)
    (define (lose msg . irritants)
      (apply error 'string-substitute msg irritants))
    (receive (open-brace close-brace)
             (case grammar
               ((braces) (values #\{ #\}))
               ((abrackets) (values #\< #\>))
               (else
                (lose "invalid grammar" dst template vals grammar)))
      (let loop ((i 0) (open-pos #f) (parts '()))
        (define (output str)
          (cond ((eqv? dst #f)
                 (cons str parts))
                ((eqv? dst #t) (display str) parts)
                (else (display str dst) parts)))
        (define (handle-close-brace/escaped pos)
          (unless (doubled-char? template pos)
            (lose "unexpected close brace" template pos))
          (loop (+ pos 2) open-pos (output (substring/shared template i (+ pos 1)))))
        (define (handle-open-brace pos)
          (cond ((doubled-char? template pos)
                 (loop (+ pos 2) #f (output
                                     (substring/shared template i (+ pos 1)))))
                (else
                 (loop (+ pos 1) pos (output
                                      (substring/shared template i pos))))))
        (if (not i)
            (if (eqv? dst #f)
                (string-concatenate-reverse parts))
            (cond (open-pos
                   (let ((close-pos (string-index template close-brace i)))
                     (unless close-pos
                       (lose "unmatched opening brace" template open-pos))
                     (cond ((doubled-char? template close-pos)
                            (loop (+ close-pos 2) open-pos parts))
                           (else
                            (loop (+ close-pos 1)
                                  #f
                                  (output (subst-one template
                                                     open-pos
                                                     close-pos
                                                     vals
                                                     lose)))))))
                  (else
                   (let ((open-pos (string-index template open-brace i))
                         (close-pos (string-index template close-brace i)))
                     (cond
                      ((not (or open-pos close-pos))
                       (loop #f #f (output (substring/shared template i))))
                      ((not open-pos)
                       (handle-close-brace/escaped close-pos))
                      ((not close-pos)
                       (handle-open-brace open-pos))
                      ((< open-pos close-pos)
                       (handle-open-brace open-pos))
                      (else
                       (handle-close-brace/escaped close-pos))))))))))

  (define (doubled-char? s i)
    (let ((c (string-ref s i)))
      (and (< (+ i 1) (string-length s))
           (char=? c (string-ref s (+ i 1))))))

  (define (subst-one template open-pos close-pos vals lose)
    (let* ((placeholder (substring/shared template (+ open-pos 1) close-pos))
           (i (string->number placeholder))
           (val (cond ((vector? vals)
                       (vector-ref vals i))
                      ((list? vals)
                       (list-ref vals i))
                      (else
                       (lose "Invalid type for replacements" vals)))))
      (cond ((string? val) val)
            ((number? val) (number->string val))
            ((char? val)   (string val))
            (else
             (call-with-string-output-port
               (lambda (port)
                 (display val port)))))))

  )
