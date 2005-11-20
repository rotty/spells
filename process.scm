;; process.scm -- System processes
;; arch-tag: fb918dfe-e2b8-4440-a8e4-1759f3fb5342

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 20, 2005 23:12

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

;; Interface for system processes

;;; Code:

;;@ Create a new child process. In the parent process, return the
;; child's process id, in the child, return #f.
(define (fork) (proc-to-be-defined))

;;@ Process id type predicate.
(define (process-id? object) (proc-to-be-defined))

;;@ Test wether two process ids are the same.
(define (process-id=? pid-a pid-b) (proc-to-be-defined))

;;@ Convert a process id object to an POSIX PID integer.
(define (process-id->integer pid) (proc-to-be-defined))

;;@ Convert a POSIX PID integer to a process id object.
(define (integer->process-id integer) (proc-to-be-defined))

;;@ Return the exit status of @1, or #f if the process is still
;; running.
(define (process-id-exit-status pid) (proc-to-be-defined))

;;@ Return the exit status of @1, or #f if the process is still
;; running.
(define (process-id-terminating-signal pid) (proc-to-be-defined))

;;@ Block until @1 has terminated.
(define (wait-for-child-process pid) (proc-to-be-defined))

;;@ Execute @1 with @2 as arguments, searching the PATH
;;environment variable. This function does not return.
(define (exec program . args) (proc-to-be-defined))

;;@ Execute @1 with @2 as arguments, without searching the PATH
;;environment variable. This function does not return.
(define (exec-file program . args) (proc-to-be-defined))

;;@ Exit with exit status @1.
(define (exit status) (proc-to-be-defined))

;;; process.scm ends here
