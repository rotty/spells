;;; lists.sls --- SRFI 1, List Libary.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells lists)
  (export
   xcons cons* make-list list-tabulate
   list-copy circular-list iota
   proper-list? circular-list? dotted-list?
   not-pair? null-list?
   list=
   first second third fourth fifth sixth seventh eighth ninth tenth
   car+cdr
   take       drop
   take-right drop-right
   take!      drop-right!
   split-at   split-at!
   last last-pair
   length length+
   append  concatenate  reverse
   append! concatenate! reverse!
   append-reverse append-reverse!
   zip unzip1 unzip2 unzip3 unzip4 unzip5
   count
   ;;map
   fold       unfold       pair-fold       reduce
   fold-right unfold-right pair-fold-right reduce-right
   append-map append-map!
   map! pair-for-each filter-map map-in-order
   filter  partition  remove
   filter! partition! remove!
   member memq memv
   assq assv
   find find-tail
   any every
   list-index
   take-while drop-while take-while!
   span break span! break!
   delete  delete-duplicates
   delete! delete-duplicates!
   alist-cons alist-copy
   alist-delete alist-delete!
   lset<= lset= lset-adjoin
   lset-union			lset-union!
   lset-intersection		lset-intersection!
   lset-difference		        lset-difference!
   lset-xor			lset-xor!
   lset-diff+intersection	        lset-diff+intersection!)

  (import (rnrs base)
          (only (rnrs lists)
                assq assv assoc
                memq memv member)
          (xitomatl srfi lists)))
