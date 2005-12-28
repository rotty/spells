;; sysutils.scm -*- scheme48-package: spells.sysutils -*-
;; Copyright (C) 2005 by Jose Antonio Ortega 

;; Author: Jose Antonio Ortega <jao@gnu.org>  
;; Start date: Wed Dec 28, 2005 00:57 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;; 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code: 

(define (extend-process-environment env)
  (let ((current-env (remove! (lambda (x) (assoc x env))
                              (current-process-environment))))
    (append env current-env)))



;;; sysutils.scm ends here
