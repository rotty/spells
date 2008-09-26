;; system.process.scm -- Dialect specific file for Scheme48
;; arch-tag: d1280a32-8ef6-40d2-b818-eeed526664cb

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Fri May 21, 2005 01:35

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

;;; Comentary:

;;@ We use the Scheme48 API, so not much to see here

;;; Code:

(scmxlate-ignore-define
 fork exec exec-file exit
 process-id?  process-id=? process-id->integer integer->process-id
 process-id-exit-status process-id-terminating-signal wait-for-child-process)

;;; system.process.scm ends here
