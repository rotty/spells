;;; fmt.sls --- R6RS library for the fmt core
;;
;; Based on fmt-scheme48.scm, which is
;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; R6RS adaption Copyright (c) 2009 Andreas Rottmann, same license.
#!r6rs

(library (spells fmt)
  (export new-fmt-state
          fmt fmt-start fmt-if fmt-capture fmt-let fmt-bind fmt-null
          fmt-ref fmt-set! fmt-add-properties! fmt-set-property!
          fmt-col fmt-set-col! fmt-row fmt-set-row!
          fmt-radix fmt-set-radix! fmt-precision fmt-set-precision!
          fmt-properties fmt-set-properties! fmt-width fmt-set-width!
          fmt-writer fmt-set-writer! fmt-port fmt-set-port!
          fmt-decimal-sep fmt-set-decimal-sep!
          copy-fmt-state
          fmt-file fmt-try-fit cat apply-cat nl fl nl-str
          fmt-join fmt-join/last fmt-join/dot fmt-join/prefix
          fmt-join/suffix fmt-join/range
          pad pad/right pad/left pad/both trim trim/left trim/both trim/length
          fit fit/left fit/both tab-to space-to wrt wrt/unshared dsp
          pretty pretty/unshared slashified maybe-slashified
          num num/si num/fit num/comma radix fix ellipses
          upcase downcase titlecase pad-char comma-char decimal-char
          with-width wrap-lines fold-lines justify
          make-string-fmt-transformer
          make-space make-nl-space display-to-string write-to-string
          fmt-columns columnar line-numbers)
  (import (except (rnrs base)
                  error
                  string-copy string->list string-for-each
                  for-each map)
          (except (rnrs unicode) string-titlecase string-upcase string-downcase)
          (rnrs control)
          (only (rnrs lists) assq)
          (rnrs arithmetic bitwise)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs mutable-pairs)
          (rnrs r5rs)
          (srfi :6 basic-string-ports)
          (srfi :69 basic-hash-tables)
          (srfi :1 lists)
          (spells opt-args)
          (except (srfi :13 strings) string-hash)
          (spells delimited-readers)
          (only (spells error) make-error-signaller)
          (spells include))

  (define (make-eq?-table) (make-hash-table eq?))
  (define error (make-error-signaller 'fmt-library))
  (define call-with-output-string call-with-string-output-port)
  
  (include-file ((spells private fmt) mantissa))
  (include-file ((spells private fmt) fmt))
  (include-file ((spells private fmt) fmt-pretty))
  (include-file ((spells private fmt) fmt-column)))
