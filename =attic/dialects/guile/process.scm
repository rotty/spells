;; system.process.scm -- Dialect specific file for Guile
;; arch-tag: 5a4f0727-ff1c-474c-bc84-2900a5b06715

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Sat May 21, 2005 00:21

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Comentary:

;;; Code:

(define (fork)
  (let ((pid (primitive-fork)))
    (if (= 0 pid)
        #f
        (integer->process-id pid))))

(define :process-id (make-record-type ":process-id" '(pid status)))
(define process-id? (record-predicate :process-id))
(define make-process-id (record-constructor :process-id))
(define process-id->integer (record-accessor :process-id 'pid))
(define process-id-status (record-accessor :process-id 'status))
(define process-id-set-status! (record-modifier :process-id 'status))

(define (process-id=? pid-a pid-b)
  (= (process-id->integer pid-a) (process-id->integer pid-b)))

(define (integer->process-id integer) (make-process-id integer #f))

(define (maybe-reap-process pid)
  (if (not (process-id-status pid))
      (let ((state (waitpid (process-id->integer pid) WNOHANG)))
        (if (> (car state) 0)
            (process-id-set-status! pid (cdr state))))))

(define (process-id-exit-status pid)
  (maybe-reap-process pid)
  (let ((status (process-id-status pid)))
    (and status (status:exit-val status))))

(define (process-id-terminating-signal pid)
  (maybe-reap-process pid)
  (let ((status (process-id-status pid)))
    (and status (status:term-sig status))))

(define (wait-for-child-process pid)
  (if (not (process-id-status pid))
      (process-id-set-status! pid (cdr (waitpid (process-id->integer pid))))))

(define (exec program . args)
  (apply execlp program args))

(define (exec-file program . args)
  (apply execl program args))

(scmxlate-ignore-define exit)

;;; system.process.scm ends here
