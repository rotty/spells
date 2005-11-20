;; assert.scm -- Tests for spells.assert
;; arch-tag: 31350b8c-d232-4765-a736-f7e41265c4fe

;; Copyright (C) 2005 by Free Software Foundation, Inc.

;; Author: Andreas Rottmann <rotty@debian.org>
;; Start date: Mon Aug 15, 2005 20:36

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

(testeez
 "assert"
 (test/equal "#t" (assert #t) #t)
 (test/equal "string" (assert "foobar") "foobar"))
 
;;; assert.scm ends here
