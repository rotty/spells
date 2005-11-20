;; process.scm -- unit tests for process.scm
;; arch-tag: 1a073af6-85ae-4ca1-86f1-03a7bbaae8f4

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Fri May 20, 2005 23:22

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Code:

(all-dialects-except mzscheme gauche)

(testeez
 "process unit tests"
 (test/eqv "fork"
           (let ((pid (fork)))
             (cond (pid
                    (wait-for-child-process pid)
                    (process-id-exit-status pid))
                   (else
                    (exit 33))))
           33))

;;; process.scm ends here
