;; packages.scm -- Utility packages
;; arch-tag: C324DB01-215B-4644-8C05-902E3404AAEA

;; Copyright (C) 2005-2007 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega <jao@gnu.org>
;; Start date: Fri May 27, 2005 23:40

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

;;; Comentary:

;; Modules providing miscelaneous utilities

;;; Code:

;;; @subsection Language facilities

;;@ @uref{http://srfi.schemers.org/srfi-39/srfi-39.html, SRFI 39} - Parameter objects.
(define-structure spells.parameter spells.srfi-39-interface
  (open scheme)
  (dialect (scheme48 (open srfi-39))
           (guile (open srfi-39)))
  (dialect (guile (re-export make-parameter with-parameters*)
                  (re-export-syntax parameterize))
           (mzscheme (files ((pure mzscheme) parameter)))))

;;@ @code{define-value} syntax.
(define-structure spells.define-values (export ((define-values)
                                                :syntax))
  (dialect (scheme48 (for-syntax (open scheme srfi-1 destructuring))))
  (dialect (scheme48 (open scheme srfi-8 primitives))
           (mzscheme (files ((pure mzscheme) define-values))))
  (files define-values))

;;@ Conditions handling.
;;
;; See @uref{http://srfi.schemers.org/srfi-34/srfi-34.html, SRFI 34},
;; @uref{http://srfi.schemers.org/srfi-35/srfi-35.html, SRFI 35} and
;; @uref{http://srfi.schemers.org/srfi-36/srfi-36.html, SRFI 36}
;; for documentation.
(define-structure spells.condition spells.condition-interface
  (dialect (guile (open ice-9.syncase srfi-1 srfi-9)
                  (replace raise)
                  (files ((pure all) srfi-35)
                         ((pure all) srfi-36)))
           (scheme48 (open conditions srfi-34 srfi-36))
           (mzscheme (open srfi-1 srfi-9 srfi-23 srfi-34)
                     (files ((pure mzscheme) srfi-35)
                            ((pure all) srfi-36))))
  (files ((pure all) condition)))

;;@ A slightly extended version of
;; @uref{http://srfi.schemers.org/srfi-23/srfi-23.html, SRFI 23}. This
;; implementation of the @ref{spells.error error,error} procedure will
;; raise a condition that can be caught with @ref{spells.condition raise,raise}.
(define-structure spells.error (export error call-error syntax-error)
  (open scheme)
  (dialect (scheme48                (open signals))
           ((gauche guile mzscheme) (open spells.condition))
           (else (open srfi-23)))
  (dialect (guile (replace error)))
  (files error))

;;@ Optional and named arguments.
(define-structure spells.opt-args spells.opt-args-interface
  (dialect (mzscheme (open (lib "defmacro.ss") spells.error)
                     (files opt-args ((pure all) opt-args)))
           (guile (open ice-9.syncase)
                  (export-syntax %let-optionals*)
                  (files opt-args ((pure all) opt-args)))))

;;@ @uref{http://srfi.schemers.org/srfi-26/srfi-26.html, SRFI 26} -
;; Notation for Specializing Parameters without Currying.
(define-structure spells.cut (export ((cut cute) :syntax))
  (open srfi-26)
  (dialect (guile (re-export-syntax cut cute))))

;;@ Procedure annotations.
(define-structure spells.annotations (export annotate-procedure procedure-annotation)
  (dialect (mzscheme (open scheme)
                     (files ((pure mzscheme) annotations)))
           (guile (files ((pure guile) annotations)))))

;;@ A simple object system.
(define-structure spells.operations spells.operations-interface
  (open scheme srfi-1 spells.error spells.annotations)
  (dialect (guile (open ice-9.syncase)
                  (export make-operation make-object)
                  (export-syntax %method-clauses->handler)))
  (files operations))

(define-structure spells.match spells.match-interface
  (open scheme spells.define-values)
  (dialect (guile (open ice-9.syncase)
                  (export-syntax literal-match guarded-match logical-match
                                 compound-match simple-match symbol??)
                  (export literal?)))
  (files match))

(define-structure spells.pacman spells.pacman-interface
  (dialect (scheme48 (open lib42.pacman))
           (mzscheme (files ((pure mzscheme) pacman)))))

;;; @subsection Data structures

;;@ Simple hash tables.
(define-structure spells.table spells.table-interface
  (open scheme srfi-16)
  (dialect (scheme48 (open srfi-23
                           (modify tables (prefix s48:))))
           (mzscheme (open spells.error)
                     (files ((pure mzscheme) table)
                            ((pure all) table)))))

;;@ Association list utilities.
(define-structure spells.alist (export acons assq-ref assv-ref assoc-ref)
  (open scheme)
  (files alist))

;;@ @uref{http://srfi.schemers.org/srfi-43/srfi-43.html, SRFI-43} -
;; Vector Library.
(define-structure spells.vector-lib spells.vector-lib-interface
  (dialect (guile (open ice-9.syncase)
                  (re-export make-vector vector vector? vector-fill!
                             vector-ref vector-length vector-set!
                             list->vector vector->list)))
  (dialect (mzscheme (files ((pure mzscheme) vector-lib))
                     (open srfi-43))
           (else (open scheme
                       srfi-8
                       spells.error
                       spells.opt-args)
                 (files vector-lib))))

;;@ Mutable cells.
(define-structure spells.cells spells.cells-interface
  (dialect (scheme48 (open cells))
           (mzscheme (files ((pure mzscheme) cells)))
           (guile (open srfi-9)
                  (files ((pure all) cells)))))

;;@ Weak pointers.
(define-structure spells.weak spells.weak-interface
  (open scheme)
  (dialect (scheme48 (open weak))
           (mzscheme (open spells.error)
                     (files ((pure mzscheme) weak)
                            ((pure all) weak)))))

(define-structure spells.define-record-type*-expander (export expand-define-record-type*
                                                              expand-define-functional-fields)
  (open scheme srfi-1 srfi-8 spells.parameter spells.error)
  (files ((pure all) defrectype)))

(define-structure spells.record-types spells.record-types-interface
  (open scheme srfi-8 srfi-9)
  (dialect (mzscheme (language mzscheme)
                     (files ((pure mzscheme) defrectype)))
           (guile (open spells.define-record-type*-expander)
                  (re-export-syntax define-record-type)
                  (re-export expand-define-record-type*)
                  (files ((pure guile) defrectype)))))

;;@ Union of Scheme 48 byte-vectors and
;; @uref{http://srfi.schemers.org/srfi-66/srfi-66.html, SRFI-66}. Once SRFI-66 is
;; finalized, and Scheme 48 implements it, the plan is to remove the
;; byte-vector aliases.
(define-structure spells.byte-vectors spells.byte-vectors-interface
  (open scheme)
  (dialect (scheme48 (open byte-vectors))
           (else (open srfi-4)
                 (files ((pure all) byte-vector-aliases)
                        ((pure all) srfi-66-ops))))
  (dialect (guile (re-export make-u8vector
                             u8vector
                             u8vector?
                             u8vector-length
                             u8vector-ref
                             u8vector-set!
                             u8vector->list
                             list->u8vector)))
  (files byte-vectors))

;;@ Bitwise arithmetic.
(define-structure spells.bitwise (export bitwise-and bitwise-ior bitwise-xor
                                         bitwise-not
                                         arithmetic-shift
                                         ;;bit-count
                                         )
  (open scheme)
  (dialect (scheme48 (open bitwise))
           (mzscheme (files ((pure mzscheme) bitwise))))
  (files bitwise))

;; @uref{http://srfi.schemers.org/srfi-74/srfi-74.html, SRFI 74} -
;; Octet-Addressed Binary Blocks.
(define-structure spells.blobs spells.blobs-interface
  (open scheme spells.cut spells.byte-vectors spells.error spells.bitwise)
  (dialect (guile (open ice-9.syncase)))
  (files blobs))


;; @uref{http://srfi.schemers.org/srfi-40/srfi-40.html, SRFI 40} -
;; A library of streams.
(define-structure spells.streams spells.streams-interface
  (open scheme srfi-1
        spells.cells
        spells.record-types
        spells.error)
  (dialect (guile (open ice-9.syncase)))
  (files streams))

;;; @subsection Text-related

;;@ ASCII encoding utilities.
(define-structure spells.ascii spells.ascii-interface
  (open scheme)
  (dialect (scheme48 (open ascii))
           (mzscheme (files ((pure mzscheme) ascii)
                            ((pure all) ascii)))))

;;@ Common-Lisp-style @code{format}.
(define-structure spells.format (export format)
  (dialect (scheme48 (open formats))
           ((mzscheme) (open srfi-48))
           (guile (open scheme))
           (else (open scheme spells.ascii spells.error srfi-38)
                 (files format))))

;;@ A simple pretty-printer for S-expressions.
(define-structure spells.pretty-print (export pp)
  (open scheme)
  (dialect (scheme48 (open pp))
           (mzscheme (files ((pure mzscheme) pretty-print)))
           (else (files pretty-print))))

;;@ Dorai Sitaram's portable
;; @uref{http://www.ccs.neu.edu/home/dorai/pregexp/pregexp.html,regular
;; expression library}.
(define-structure spells.pregexp spells.pregexp-interface
  (open scheme srfi-1 spells.error)
  (files pregexp))

;;; @subsection Input/Output

;;@ Port utilities.
(define-structure spells.port spells.port-interface
  (open scheme)
  (dialect
   (scheme48 (open i/o
                   (subset features (force-output))
                   (subset posix-files (file-options file-options-on? file-mode open-file))))
   (mzscheme (files ((pure mzscheme) port)
                    ((pure all) port)))
   (guile (open ice-9.syncase)
          (re-export current-error-port
                     with-output-to-port
                     with-input-from-port
                     force-output)
          (files ((pure guile) port)))))

;;@ Read and write blocks of data on ports.
(define-structure spells.block-io (export read-block write-block write-string)
  (open scheme)
  (dialect (scheme48 (open i/o))
           (mzscheme (files ((pure mzscheme) block-io)))))

(define-structure spells.delimited-readers spells.delimited-readers-interface
  (open scheme
        srfi-8
        srfi-13
        srfi-14
	spells.byte-vectors
        spells.error
	spells.ascii
        spells.opt-args)
  (dialect (mzscheme (files ((pure mzscheme) delimited-readers)
                            ((pure all) skip-char-set)))
           (scheme48 (files ((pure scheme48) delimited-readers))))
  (files ((pure all) delimited-readers)))

(define-structure spells.field-reader spells.field-reader-interface
  (open scheme srfi-8 srfi-13 srfi-14
        spells.error
        spells.opt-args
	spells.delimited-readers
        spells.pregexp)
  (dialect (mzscheme (files ((pure mzscheme) field-reader))))
  (files ((pure all) field-reader)))

;;; @subsection Operating system interface

(define-structure spells.time-lib spells.time-lib-interface
  (dialect (scheme48 (files ((pure scheme48) srfi-19)
                            ((pure all) time-lib)))
           (mzscheme (open srfi-19)
                     (files ((pure all) time-lib)))
           (guile (open srfi-19)
                  (re-export time-duration ;; Constants
                             time-monotonic
                             time-process
                             time-tai
                             time-thread
                             time-utc
                             ;; Current time and clock resolution
                             current-date
                             current-julian-day
                             current-modified-julian-day
                             current-time
                             time-resolution
                             ;; Time object and accessors
                             make-time
                             time?
                             time-type
                             time-nanosecond
                             time-second
                             set-time-type!
                             set-time-nanosecond!
                             set-time-second!
                             copy-time
                             ;; Time comparison procedures
                             time<=?
                             time<?
                             time=?
                             time>=?
                             time>?
                             ;; Time arithmetic procedures
                             time-difference
                             time-difference!
                             add-duration
                             add-duration!
                             subtract-duration
                             subtract-duration!
                             ;; Date object and accessors
                             make-date
                             date?
                             date-nanosecond
                             date-second
                             date-minute
                             date-hour
                             date-day
                             date-month
                             date-year
                             date-zone-offset
                             date-year-day
                             date-week-day
                             date-week-number
                             ;; Time/Date/Julian Day/Modified Julian Day converters
                             date->julian-day
                             date->modified-julian-day
                             date->time-monotonic
                             date->time-tai
                             date->time-utc
                             julian-day->date
                             julian-day->time-monotonic
                             julian-day->time-tai
                             julian-day->time-utc
                             modified-julian-day->date
                             modified-julian-day->time-monotonic
                             modified-julian-day->time-tai
                             modified-julian-day->time-utc
                             time-monotonic->date
                             time-monotonic->time-tai
                             time-monotonic->time-tai!
                             time-monotonic->time-utc
                             time-monotonic->time-utc!
                             time-tai->date
                             time-tai->julian-day
                             time-tai->modified-julian-day
                             time-tai->time-monotonic
                             time-tai->time-monotonic!
                             time-tai->time-utc
                             time-tai->time-utc!
                             time-utc->date
                             time-utc->julian-day
                             time-utc->modified-julian-day
                             time-utc->time-monotonic
                             time-utc->time-monotonic!
                             time-utc->time-tai
                             time-utc->time-tai!
                             ;; Date to string/string to date converters.
                             date->string
                             string->date)
                  (files ((pure all) time-lib)))))

(define-structure spells.pathname spells.pathname-interface
  (open scheme srfi-1 srfi-8 srfi-13 srfi-14
        spells.error
        spells.opt-args
        spells.record-types
        spells.operations)
  (dialect (mzscheme (files ((pure mzscheme) pathname))))
  (files pathname))

;;@ File system interface.
(define-structure spells.filesys spells.filesys-interface
  (open scheme srfi-1 srfi-8
        spells.condition
        spells.pathname
        spells.time-lib)
  (dialect (mzscheme (files ((pure mzscheme) filesys)
                            ((pure all) filesys)))))

;;@ Miscellaneous procedures providing access to various bits of
;; information regarding the host running the scheme implementation.
(define-structure spells.sysutils spells.sysutils-interface
  (open scheme spells.error)
  (dialect (scheme48 (open posix-process-data posix-platform-names))
           (mzscheme (files ((pure mzscheme) sysutils)))))

;;@ Process interface.
(define-structure spells.process spells.process-interface
  (open scheme srfi-1 srfi-13 spells.pathname spells.delimited-readers)
  (dialect (scheme48 (open posix-processes))
           (guile (re-export exit))
           (mzscheme (open spells.record-types srfi-8 spells.error)
                     (files ((pure mzscheme) process))))
  (files ((pure all) process)))

;;; @subsection Unclassified

(define-structure spells.logging spells.logging-interface
  (open scheme srfi-1
        spells.time-lib
        spells.error
        spells.opt-args
        spells.record-types)
  (files logging))

;;@ Assert the truth of an expression.
(define-structure spells.assert (export ((assert) :syntax) cerr cout)
  (open scheme
        spells.error
        spells.port)
  (dialect (guile (open ice-9.syncase)))
  (files assert))

;;@ Hash functions.
(define-structure spells.hash (export descriptor-hash gc-stamp)
  (open scheme)
  (dialect (scheme48 (open (subset primitives (memory-status))
                           (subset architecture (memory-status-option))
                           enumerated))
           (mzscheme (files ((pure mzscheme) hash)))))

;;@ Stuff that doesn't fit somewhere else.
(define-structure spells.misc spells.misc-interface
  (open scheme)
  (dialect (guile (re-export thunk? and-map or-map sort-list))
           (mzscheme (open (lib "list.ss"))
                     (files ((pure mzscheme) misc)))
           (scheme48 (open srfi-23
                           (subset primitives (unspecific eof-object))
                           i/o
                           closures templates bitwise byte-vectors architecture
                           posix-process-data
                           threads
                           sort))))


;;; packages.scm ends here

