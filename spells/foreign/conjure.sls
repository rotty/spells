#!r6rs
;;; conjure.sls --- Conjure tasks for the FFI

;; Copyright (C) 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells foreign conjure)
  (export foreign-conjure-tasks)
  (import (rnrs)
          (conjure cc)
          (conjure hostinfo)
          (conjure dsl))

(define (foreign-conjure-tasks)
  (task configure
    (configure
     (produce `((("spells" "foreign") "config.sls")
                <= (("spells" "foreign") "config.sls.in")))
     (fetchers (cc-fetcher 'cc)
               (hostinfo-fetcher))))
  
  (task cc (cc-conf))
       
)

)

;; Local Variables:
;; scheme-indent-styles: (conjure-dsl)
;; End:
