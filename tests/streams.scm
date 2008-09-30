;; streams.scm --- unit tests for spells.streams

;; Copyright (C) 2006 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann
;; Start date: Sat May 20 20:01:51 CEST 2006

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

(let ((s (stream 1 2 3)))
  (testeez "basic"
    (test/equal "car" (stream-car s) 1)))

;;; streams.scm ends here
