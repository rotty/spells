;;; args-fold.sls --- Slightly extended variant of SRFI 37

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (c) 2002 Anthony Carrico

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This is a slight extension of SRFI-37, unimaginatively called
;; `args-fold*', which requires that the option processor returns an
;; additional value which, if true, causes option processing to be
;; stopped, and all further command line arguments are considered
;; operands.

;; The signature of an option processor is hence:
;;
;; (option-processor option name arg seed ...) => done? seed ...
;;

;;; Code:
#!r6rs

(library (spells args-fold)
  (export option
          args-fold*
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor)
  (import (rnrs base)
          (srfi :9 records))


(define-record-type option-type
  (option names required-arg? optional-arg? processor)
  option?
  (names option-names)
  (required-arg? option-required-arg?)
  (optional-arg? option-optional-arg?)
  (processor option-processor))

(define (args-fold* args
                    options
                    unrecognized-option-proc
                    operand-proc
                    . seeds)
  (define (find l ?)
    (cond ((null? l) #f)
          ((? (car l)) (car l))
          (else (find (cdr l) ?))))
  ;; ISSUE: This is a brute force search. Could use a table.
  (define (find-option name)
    (find options (lambda (option)
                    (find
                     (option-names option)
                     (lambda (test-name)
                       (equal? name test-name))))))
  (define (scan-short-options index shorts args seeds)
    (if (= index (string-length shorts))
        (scan-args args seeds)
        (let* ((name (string-ref shorts index))
               (option (or (find-option name)
                           (option (list name)
                                   #f
                                   #f
                                   unrecognized-option-proc))))
          (cond ((and (< (+ index 1) (string-length shorts))
                      (or (option-required-arg? option)
                          (option-optional-arg? option)))
                 (process-arg+iterate option
                                      name
                                      (substring shorts (+ index 1) (string-length shorts))
                                      args
                                      seeds))
                ((and (option-required-arg? option)
                      (pair? args))
                 (process-arg+iterate option name (car args) (cdr args) seeds))
                (else
                 (let-values
                     (((done? . seeds) (apply (option-processor option)
                                              option
                                              name
                                              #f
                                              seeds)))
                   (if done?
                       (scan-operands args seeds)
                       (scan-short-options (+ index 1)
                                           shorts
                                           args
                                           seeds))))))))
  (define (scan-operands operands seeds)
    (if (null? operands)
        (apply values seeds)
        (let-values ((seeds (apply operand-proc
                                   (car operands)
                                   seeds)))
          (scan-operands (cdr operands) seeds))))
  (define (process-arg+iterate option name arg args seeds)
    (let-values (((done? . seeds) (apply (option-processor option)
                                         option
                                         name
                                         arg
                                         seeds)))
      (if done?
          (scan-operands args seeds)
          (scan-args args seeds))))
  (define (scan-args args seeds)
    (if (null? args)
        (apply values seeds)
        (let ((arg (car args))
              (args (cdr args)))
          ;; NOTE: This string matching code would be simpler
          ;; using a regular expression matcher.
          (cond
           ( ;; (rx bos "--" eos)
            (string=? "--" arg)
            ;; End option scanning:
            (scan-operands args seeds))
           ( ;;(rx bos
            ;;    "--"
            ;;    (submatch (+ (~ "=")))
            ;;    "="
            ;;    (submatch (* any)))
            (and (> (string-length arg) 4)
                 (char=? #\- (string-ref arg 0))
                 (char=? #\- (string-ref arg 1))
                 (not (char=? #\= (string-ref arg 2)))
                 (let loop ((index 3))
                   (cond ((= index (string-length arg))
                          #f)
                         ((char=? #\= (string-ref arg index))
                          index)
                         (else
                          (loop (+ 1 index))))))
            ;; Found long option with arg:
            => (lambda (=-index)
                 (let* ((name (substring arg 2 =-index))
                        (option-arg (substring arg
                                               (+ =-index 1)
                                               (string-length arg)))
                        (option (or (find-option name)
                                    (option (list name)
                                            #t
                                            #f
                                            unrecognized-option-proc))))
                   (process-arg+iterate option name option-arg args seeds))))
           ( ;;(rx bos "--" (submatch (+ any)))
            (and (> (string-length arg) 3)
                 (char=? #\- (string-ref arg 0))
                 (char=? #\- (string-ref arg 1)))
            ;; Found long option:
            (let* ((name (substring arg 2 (string-length arg)))
                   (option (or (find-option name)
                               (option
                                (list name)
                                #f
                                #f
                                unrecognized-option-proc))))
              (if (and (option-required-arg? option)
                       (pair? args))
                  (process-arg+iterate option name (car args) (cdr args) seeds)
                  (process-arg+iterate option name #f args seeds))))
           ( ;; (rx bos "-" (submatch (+ any)))
            (and (> (string-length arg) 1)
                 (char=? #\- (string-ref arg 0)))
            ;; Found short options
            (let ((shorts (substring arg 1 (string-length arg))))
              (scan-short-options 0 shorts args seeds)))
           (else
            (let-values ((seeds (apply operand-proc arg seeds)))
              (scan-args args seeds)))))))
  (scan-args args seeds))

)
