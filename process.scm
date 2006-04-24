;; process.scm -- System processes
;; arch-tag: fb918dfe-e2b8-4440-a8e4-1759f3fb5342

;; Copyright (C) 2005-2006 by Free Software Foundation, Inc.

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

;;@ Run @2 with the environment @1 and arguments @3
;; asynchronously. The return value is a process descriptor to be
;; passed to @ref{spells.process wait-for-process,wait-for-process},
;; @ref{spells.process close-process-ports,}, @ref{spells.process
;; process-input,process-input}, @ref{spells.process,
;; process-output,process-output}.
(define (spawn-process env prog . args)
  (proc-to-define))

;;@ Wait for termination of @1, which must have been created by
;; @ref{spells.process spawn-process,spawn-process}.
(define (wait-for-process process)
  (proc-to-define))

;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated).
(define (run-process env prog . args)
  (proc-to-define))

;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated). The return values are
;; the exit status, terminating signal (if the process has been
;; terminated by a signal and the standard output captured in a
;; string.
;;
;; Using @code{srfi-8}, @0 can be used as follows:
;; @lisp
;; (receive (status signal output) (run-process/string #f "cat /etc/hostname")
;;   ...)
;; @end lisp
(define (run-process/string env prog . args)
  (proc-to-define))

;;@stop

(define (open-process-input env prog . args)
  (proc-to-define))

(define (open-process-output env prog . args)
  (proc-to-define))

(define (call-with-process-output env prog+args receiver)
  (proc-to-define))

(define (run-process/lines env prog . args)
  (proc-to-define))

(define (run-process/sexps env prog . args)
  (proc-to-define))

;;; process.scm ends here
