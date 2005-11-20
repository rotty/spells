;; parameter.scm -- Unit tests for spells.parameter (SRFI 39)
;; arch-tag: 9ea61de1-4c7a-4b74-afad-65ca93ff97cd

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri Jul 30, 2005 22:43

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
 (test/equal "mutation" (let ((p (make-parameter 5))) (p 10) (p)) 10)
 (test/equal "parameterize" (let ((p (make-parameter 5)))
                              (list
                               (parameterize ((p 10))
                                 (p))
                               (p))) '(10 5)))

;;; parameter.scm ends here
