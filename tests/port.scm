;; port.scm --- unit tests for port.scm
;; arch-tag: 5a7b0529-6981-4c91-90f7-c8d7b2637264

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Sun Sep 18, 2005 12:36

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

;;; Code:

(testeez
 "basic"
 (test-true "current-error-port" (output-port? (current-error-port)))
 (test/equal "with-input-from-port"
             (let ((port (open-input-string "(1 2 3)")))
               (with-input-from-port port read))
             '(1 2 3))
 (test/equal "with-output-to-port"
             (let ((port (open-output-string)))
               (with-output-to-port port
                 (lambda () (write 123)))
               (get-output-string port))
             "123"))

;;; ports.scm ends here
