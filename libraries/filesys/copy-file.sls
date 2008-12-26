;;; copy-file.sls --- copy-file implemented in vanilla R6RS

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Note that this implementation does not (yet) preserve file
;; permissions (as there's no spells API to access them).

;;; Code:
#!r6rs

(library (spells filesys copy-file)
  (export copy-file)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs io ports)
          (spells pathname))

  (define x->f x->namestring)

  (define (copy-file src-pathname dst-pathname)
    (let* ((buf-size 4000)
           (buffer (make-bytevector buf-size 0)))
      (call-with-port (open-file-input-port (x->f src-pathname))
        (lambda (in-port)
          (call-with-port (open-file-output-port (x->f dst-pathname))
            (lambda (out-port)
              (let loop () 
                (let ((n (get-bytevector-n! in-port buffer 0 buf-size))) 
                  (cond ((not (eof-object? n)) 
                         (put-bytevector out-port buffer 0 n) 
                         (loop)))))))))))
)
