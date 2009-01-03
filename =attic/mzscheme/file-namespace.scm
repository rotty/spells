;; file-namespace.scm -- Language module for file.scm
;; arch-tag: EA46317A-DB27-4DA1-B469-48B745A0D22E

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega <jao@macavitysw.com>
;; Start date: Thu May 12, 2005 20:56

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

;; Auxiliar language definition used by file.scm to circumvent name
;; classes.

;;; Code:

(module file-namespace mzscheme
  (require (lib "file.ss"))
  (provide (all-from-except mzscheme current-directory)
           (all-from-except (lib "file.ss") find-files normalize-path)
           (rename current-directory mzscheme-current-directory)))


;;; file-namespace.scm ends here
