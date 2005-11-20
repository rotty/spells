;; port.scm -- More port procedures
;; arch-tag: b3caaea7-c44a-4bd5-87af-df643db94d62

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Aug  1, 2005 21:41

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

;;@ Returns the current error port.
(define (current-error-port) (proc-to-define))

;;@ Call @4 with @1, @2 and @3 as @code{(current-input-port)},
;; @code{(current-output-port)} and @code{(current-error-port)},
;; respectively.
(define (with-current-ports in out error thunk)
  (proc-to-define))

;;@ Force the buffers of port to be flushed, if it is an open output
;; port.
(define (force-output port)
  (proc-to-define))

;;@ Open an output port corresponding to file @1 using the options @2.
(define (open-output-file/options pathname options)
  (proc-to-define))

;;@args name...
;;
;; Constructor for file options. The arguments may be a combination of
;; @code{create}, @code{exclusive}, @code{truncate}, @code{append}.
(define-syntax file-options
  (syntax-rules ()
    RULES-TO-DEFINE))
