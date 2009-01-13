;;; condition.scm --- Additional condition types and utilities.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Comentary:

;;
;; These are common condition types which can be shared among
;; libraries (instead of each re-defining them).

;;; Code:


(define-condition-type &parser-error &error
  make-parser-error parser-error?
  (port parser-error-port))

(define-condition-type &stacked &condition
  make-stacked-condition stacked-condition?
  (next next-condition))

  ;; Condition printing code taken from Ikarus --> it's GPLv3, need to
  ;; ask for relicencing permission
  (define (print-simple-condition x p)
    (let* ((rtd (record-rtd x))
           (rf (let l ((rtd rtd) (accum '()))
                 (if rtd
                     (l (record-type-parent rtd)
                        (cons
                         (cons rtd (record-type-field-names rtd))
                         accum))
                     (remp (lambda (a) (zero? (vector-length (cdr a))))
                           accum))))
           (rf-len (apply + (map vector-length
                                 (map cdr rf)))))
      (let ((name (record-type-name rtd)))
        (display name p))
      (case rf-len
        ((0) (newline p))
        ((1)
         (display ": " p)
         (write ((record-accessor (caar rf) 0) x) p)
         (newline p))
        (else
         (display ":\n" p)
         (for-each
          (lambda (a)
            (let f ((i 0) (rtd (car a)) (v (cdr a)))
              (unless (= i (vector-length v))
                (display "       " p)
                (display (vector-ref v i) p)
                (display ": " p)
                (write ((record-accessor rtd i) x) p)
                (newline p)
                (f (+ i 1) rtd v))))
          rf)))))

(define display-condition
  (case-lambda
    ((x p)
     (cond
      ((condition? x)
       (let ((ls (simple-conditions x)))
         (if (null? ls)
             (display "Condition object with no further information\n" p)
             (begin
               (display " Condition components:\n" p)
               (let f ((ls ls) (i 1))
                 (unless (null? ls)
                   (display "   " p)
                   (display i p)
                   (display ". " p)
                   (print-simple-condition (car ls) p)
                   (f (cdr ls) (+ i 1))))))))
      (else
       (display " Non-condition object: " p)
       (write x p)
       (newline p))))
    ((x)
     (display-condition x (current-output-port)))))


;; Utilities for code below. This whole section should probably be
;; moved to its own library.

(define (write-string s port)
  (put-string port s))

;; Code below taken from scheme48 1.8, Copyright (c) 1993-2008 Richard
;; Kelsey and Jonathan Rees. Licensed under the new-style BSD license.

(define (limited-write obj port max-depth max-length)
  (let recur ((obj obj) (depth 0))
    (if (and (= depth max-depth)
	     (not (or (boolean? obj)
		      (null? obj)
		      (number? obj)
		      (symbol? obj)
		      (char? obj)
		      (string? obj))))
	(display "#" port)
	(call-with-current-continuation
	  (lambda (escape)
	    (recurring-write obj port
	      (let ((count 0))
		(lambda (sub)
		  (if (= count max-length)
		      (begin (display "---" port)
			     (write-char
			      (if (or (pair? obj) (vector? obj))
				  #\)
				  #\})
			      port)
			     (escape #t))
		      (begin (set! count (+ count 1))
			     (recur sub (+ depth 1))))))))))))

(define (recurring-write obj port recur)
  (cond ((pair? obj) (write-list obj port recur))
        ((vector? obj) (write-vector obj port recur))
        (else
         (write obj port))))

(define (write-list obj port recur)
  (cond ((quotation? obj)
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((not (pair? l))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

;;; condition.scm ends here

