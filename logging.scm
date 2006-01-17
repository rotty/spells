;; -*- mode: scheme; scheme48-package: spells.logging; -*-
;;
;; Copyright (C) 2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Tue Jan 17 13:50:56 CET 2006

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

;;; Data types

(define-record-type* logger
  (make-logger name parent)
  ((threshold #f)
   (propagate? #t)
   (filters '())
   (handlers '())))

(define-record-discloser logger
  (lambda (l)
    (list 'logger (logger-name l))))

(define-record-type* log-handler
  (make-log-handler proc threshold filters)
  ())

(define-record-type* log-entry
  (make-log-entry logger level (%object))
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

(define (log logger entry)
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
                   (log parent entry)))))))

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

(define (make-port-log-handler port formatter threshold filters)
  (make-log-handler (lambda (entry) (formatter entry port)) threshold filters))

(define (default-log-formatter entry port)
  (display #\( port)
  (do ((name (logger-name (log-entry-logger entry)) (cdr name)))
      ((null? name))
    (display (car name) port)
    (if (not (null? (cdr name)))
        (display #\. port)))
  (display ": [" port)
  (display (log-entry-level-name)  port)
  (display "] " port)
  (display (log-entry-object entry) port)
  (display #\) port)
  (newline port))

;;; Global state

(define *root-logger* (make-logger '(root) #f))

(define *default-config*
  `((port ,(current-error-port)
          (formatter ,default-log-formatter))))

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
      (log logger (make-log-entry logger level obj)))))


;; <config> -->
;;            <handler-clause>
;;          | (threshold <level>)
;; <handler-clause> -->
;;            (port <expression> <handler-detail>*)
;; <handler-detail> -->
;;          | (threshold <level>)
;;          | (formatter <expression>)
;;
(define/optional-args (configure-logger name (optional (config *default-config*)))
  (receive (handlers threshold) (eval-logger-details config)
    (let ((logger (get-logger name)))
      (set-logger-handlers! logger handlers)
      (set-logger-threshold! logger threshold))))

(define (eval-logger-details details)
  (let loop ((details details) (handlers '()) (threshold #f))
    (if (null? details)
        (values handlers (numeric-level threshold))
        (match (car details)
          ((list-rest 'port port details)
           (receive (formatter threshold) (eval-handler-details details)
             (loop (cdr details)
                   (cons (make-port-log-handler port formatter threshold '()) handlers)
                   threshold)))
          ((list 'threshold threshold)
           (loop (cdr details) handlers threshold))
          (else
           (error "invalid logger configuration clause" (car details)))))))

(define (eval-handler-details details)
  (let loop ((details details) (formatter default-log-formatter) (threshold #f))
    (if (null? details)
        (values formatter (numeric-level threshold))
        (match (car details)
          ((list 'threshold threshold) (loop (cdr details) formatter threshold))
          ((list 'formatter formatter) (loop (cdr details) formatter threshold))
          (else
           (error "invalid log handler configuration clause" (car details)))))))
