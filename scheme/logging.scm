;;; logging.scm --- Logging library.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


;;; Data types

(define-record-type* logger
  (make-logger name parent)
  ((threshold #f)
   (propagate? #t)
   (filters '())
   (handlers '())))

(define-record-type* log-handler
  (%make-log-handler proc threshold filters)
  ())

(define/optional-args (make-log-handler proc (optional (threshold #f) (filters '())))
  (%make-log-handler proc (numeric-level threshold) filters))

(define-record-type* log-entry
  (make-log-entry logger level time (%object))
  ())

(define (log-entry-object entry)
  (let ((obj (log-entry-%object entry)))
    (if (procedure? obj)
        (let ((value (obj)))
          (set-log-entry-%object! entry value)
          value)
        obj)))

(define (log-entry-level-name entry)
  (let ((level (log-entry-level entry)))
    (or (any (lambda (entry)
               (and (= (cdr entry) level) (car entry)))
             *builtin-levels*)
        level)))

;;; Internal functions

(define (passes? threshold filters entry)
  (and (or (eqv? threshold #f)
           (>= (log-entry-level entry) threshold))
       (every (lambda (filter) (filter entry)) filters)))

(define (handle handler entry)
  (if (passes? (log-handler-threshold handler)
               (log-handler-filters handler)
               entry)
      ((log-handler-proc handler) entry)))

(define (do-log logger entry)
  (define (do-handle)
    (for-each (lambda (handler) (handle handler entry))
              (logger-handlers logger)))
  
  (let ((threshold (logger-threshold logger))
        (parent (logger-parent logger)))
    (if (passes? threshold (logger-filters logger) entry)
        (cond ((eqv? parent #f) ;; root logger?
               (do-handle))
              (else
               (do-handle)
               (if (logger-propagate? logger)
                   (do-log parent entry)))))))

;; The nodes in the logger tree
(define node-name car)
(define node-logger cadr)
(define node-children cddr)
(define (add-child! node name logger)
  (let ((child (list name logger)))
    (set-cdr! (cdr node) (cons child (node-children node)))
    child))

(define (get-logger name)
  (cond ((not name)
         *root-logger*)
        ((symbol? name)
         (get-logger (list name)))
        ((list? name)
         (let loop ((name-rest name) (node *logger-tree*))
           (if (null? name-rest)
               (node-logger node)
               (cond ((assq (car name-rest) (node-children node))
                      => (lambda (node)
                           (loop (cdr name-rest) node)))
                     (else
                      (loop (cdr name-rest)
                            (add-child! node
                                        (car name-rest)
                                        (make-logger name (node-logger node)))))))))
        (else
         (error "invalid logger name" name))))

;;; Builtin handlers & formatters

(define (default-log-formatter entry port)
  (display #\( port)
  (do ((name (logger-name (log-entry-logger entry)) (cdr name)))
      ((null? name))
    (display (car name) port)
    (if (not (null? (cdr name)))
        (display #\. port)))
  (display ": [" port)
  (display (log-entry-level-name entry)  port)
  (display "] " port)
  (display (log-entry-object entry) port)
  (display #\) port)
  (newline port))

;;; Global state

(define *root-logger* (make-logger '(root) #f))

(define (*default-config*)
  `((handlers ,(make-log-handler
                (lambda (entry) (default-log-formatter entry (current-output-port)))))))

;; This is mutated, need to think about thread safety
(define *logger-tree* (list 'root *root-logger*))

(define *builtin-levels* '((debug    . 10)
                           (info     . 20)
                           (warning  . 30)
                           (error    . 40)
                           (critical . 50)))

(define (numeric-level level)
  (cond ((and (symbol? level)
              (assq level *builtin-levels*))
         => cdr)
        ((or (eqv? level #f) (integer? level))
         level)
        (else
         (error "invalid level" level))))

(define (config-ref config key default)
  (cond ((assq key config) => cdr)
        (else default)))

(define (config-ref* config key default)
  (cond ((assq key config) => cadr)
        (else default)))

;;; Public interface

;;@ Create a logging procedure.
;;
;; @1 must be a list of symbols that form a path to the logger that
;; should be used; @code{#f} and @code{'()} refer to the root
;; logger. The logging level at which the returned procedure will
;; issue its log entrys is given by @2; it may be either an integer,
;; or one of the builtin levels @code{'debug}, @code{'info},
;; @code{'warning}, @code{'error}, @code{'critical}.
;;
(define (make-log name level)
  (let ((level (numeric-level level))
        (logger (get-logger name)))
    (lambda (obj)
      (do-log logger (make-log-entry logger level (current-time) obj)))))


;; <config> --> (<config-clause>*)
;; <config-clause> -->
;;          | (handlers <handler>*)
;;          | (threshold <int>)
;;          | (propagate? <bool>)
(define/optional-args (configure-logger name (optional (config (*default-config*))))
  (let ((logger (get-logger name)))
    (set-logger-handlers! logger (config-ref config 'handlers '()))
    (set-logger-threshold! logger (numeric-level (config-ref* config 'threshold #f)))
    (set-logger-propagate?! logger (config-ref* config 'propagate? #t))))


;; Must go last, since it may expand into an expression
(define-record-discloser logger
  (lambda (l)
    (list 'logger (logger-name l))))

