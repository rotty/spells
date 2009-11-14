;;; stexidoc.sls --- stexidoc extractors

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells private stexidoc)
  (export foreign-extractors)
  (import (rnrs)
          (spells match)
          (stexidoc extract)
          (stexidoc reader))

(define (foreign:define-extractor form)
  (match (cdr (strip-non-forms form))
    ((name ('make-pointer-c-getter type))
     `((procedure (^ (name ,name) (arguments pointer offset)))))
    (else
     #f)))

(define foreign-extractors
  (extend-extractors usual-spedl-extractors
                     `((define . ,foreign:define-extractor))))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1))
;; End:
