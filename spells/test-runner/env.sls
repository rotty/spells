#!r6rs
;; Copyright (C) 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (spells test-runner env)
  (export this-directory
          test-environment)
  (import (rnrs)
          (srfi :39 parameters)
          (spells pathname))

  (define this-directory
    (make-parameter (->namestring
                     (pathname-with-file (->pathname (car (command-line))) #f))))

  (define test-environment (make-parameter #f)))
