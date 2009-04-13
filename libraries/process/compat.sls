;;; compat.sls --- OS processes, compat library template

;; Copyright (C) 2005-2006, 2008, 2009 Andreas Rottmann

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Comentary:

;; Interface for system processes

;;; Code:
#!r6rs

(library (spells process compat)
  (export spawn-process
          wait-for-process)
  (import (rnrs base))

  ;;@ Run @2 with the environment @1 and arguments @3
  ;; asynchronously. The return value is a process descriptor to be
  ;; passed to @ref{spells.process wait-for-process,wait-for-process},
  ;; @ref{spells.process close-process-ports,close-process-ports},
  ;; @ref{spells.process process-input,process-input},
  ;; @ref{spells.process, process-output,process-output}.
  (define (spawn-process env prog . args)
    (proc-to-define))

  ;;@ Wait for termination of @1, which must have been created by
  ;; @ref{spells.process spawn-process,spawn-process}. The return values
  ;; are the exit status and the terminating signal (in case the process
  ;; has been terminated by a signal, or false otherwise).
  (define (wait-for-process process)
    (proc-to-define))
)

;;; process.scm ends here
