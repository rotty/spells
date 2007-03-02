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
  (dialect (guile (re-export make-parameter parameterize))
           (mzscheme (files ((pure mzscheme) parameter))))
  (files parameter))

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
  (open scheme srfi-1 srfi-9 srfi-23)
  (dialect (guile (open ice-9.syncase))
           (scheme48 (open conditions srfi-34 srfi-36))
           (mzscheme (open srfi-34)
                     (files ((pure all) srfi-35)
                            ((pure all) srfi-36))))
  (files ((pure all) condition)))

;;@ A slightly extended version of
;; @uref{http://srfi.schemers.org/srfi-23/srfi-23.html, SRFI 23}. This
;; implementation of the @ref{spells.error error,error} procedure will
;; raise a condition that can be caught with @ref{spells.condition raise,raise}.
(define-structure spells.error (export error call-error syntax-error)
  (open scheme)
  (dialect (scheme48 (open signals))
           ((gauche guile mzscheme)
            (language scheme)
            (open spells.condition))
           (else (open srfi-23)))
  (files error))

;;@ Optional and named arguments.
(define-structure spells.opt-args spells.opt-args-interface
  (open scheme spells.error)
  (dialect (mzscheme (open (lib "defmacro.ss")))
           (guile (open ice-9.optargs)))
  (files opt-args ((pure all) opt-args)))


;;@ @uref{http://srfi.schemers.org/srfi-26/srfi-26.html, SRFI 26} -
;; Notation for Specializing Parameters without Currying.
(define-structure spells.cut (export ((cut cute) :syntax))
  (dialect (guile (open scheme))
           (else (open srfi-26)))
  (files cut))

;;@ A simple object system.
(define-structure spells.operations spells.operations-interface
  (open scheme spells.error)
  (files operations))

(define-structure spells.match spells.match-interface
  (open scheme spells.define-values)
  (files match))

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
  (open scheme
        spells.error)
  (dialect (mzscheme (files ((pure mzscheme) vector-lib))
                     (open srfi-43))
           (else (files vector-lib))))

;;@ Mutable cells.
(define-structure spells.cells spells.cells-interface
  (dialect (scheme48 (open cells))
           (mzscheme (files ((pure mzscheme) cells)))))

;;@ Weak pointers.
(define-structure spells.weak spells.weak-interface
  (open scheme)
  (dialect (scheme48 (open weak))
           (mzscheme (open spells.error)
                     (files ((pure mzscheme) weak)
                            ((pure all) weak)))))

(define-structure spells.define-record-type*-expander (export expand-define-record-type*
                                                              expand-define-functional-fields)
  (open scheme srfi-1 srfi-8 spells.match spells.parameter spells.error)
  (files ((pure all) defrectype)))

(define-structure spells.record-types spells.record-types-interface
  (open scheme srfi-8 srfi-9)
  (dialect (mzscheme (language mzscheme)
                     (files ((pure mzscheme) defrectype)))))

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
  (files blobs))


;; @uref{http://srfi.schemers.org/srfi-40/srfi-40.html, SRFI 40} -
;; A library of streams.
(define-structure spells.streams spells.streams-interface
  (open scheme srfi-1
        spells.cells
        spells.record-types
        spells.error)
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
                    ((pure all) port)))))

;;@ Read and write blocks of data on ports.
(define-structure spells.block-io (export read-block write-block write-string)
  (open scheme)
  (dialect (scheme48 (open i/o))
           (mzscheme (files ((pure mzscheme) block-io)))))


;;; @subsection Operating system interface

(define-structure spells.time-lib spells.time-lib-interface
  (dialect (scheme48 (files ((pure scheme48) srfi-19)
                            ((pure all) time-lib)))
           (mzscheme (open srfi-19)
                     (files ((pure all) time-lib)))))

(define-structure spells.pathname spells.pathname-interface
  (open scheme srfi-1 srfi-8 srfi-13 srfi-14
        spells.error
        spells.opt-args
        spells.record-types
        spells.operations)
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
  (all-dialects-except mzscheme)
  (open scheme srfi-1)
  (dialect (scheme48 (open posix-process-data posix-platform-names))))

;;@ Process interface.
(define-structure spells.process spells.process-interface
  (all-dialects-except mzscheme)
  (open scheme)
  (dialect (scheme48 (open posix-processes))
           (guile (re-export exit)))
  (files process))

;;; @subsection Unclassified

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

