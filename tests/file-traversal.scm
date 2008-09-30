;; file-traversal.scm -- unit tests for find functions in file.scm
;; arch-tag: 0e1f0c08-3053-11d9-8bf0-00404513c0a4

;; Copyright (C) 2004, 2005 by Free Software Foundation, Inc.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sun Nov 07, 2004 01:22

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

(define (contains? f1 f2)
  (and (list? f1) (list? f2)
       (let loop ((f f2))
         (cond ((null? f) #t)
               ((member (car f) f1) (loop (cdr f)))
               (else #f)))))

(define (set-equal? f1 f2) (and (contains? f1 f2) (contains? f2 f1)))

(define *builddir* "@abs_top_builddir@")

(testeez
 "Directory traversals"
 (test-false "find-exec-path 1" (find-exec-path ".abracadabra.abracadabra."))
 (test/equal "find-exec-path 2"
             (find-exec-path (file-basename "@ac_spells_scheme_prog@"))
             "@ac_spells_scheme_prog@")

 (test-define "Aux" f1 (find-files *builddir* ".+" #f))
 (test-define "Aux" f2 (find-files *builddir* "boot.+" #f))
 (test-define "Aux" f3 (find-files *builddir* ".+" #t))
 (test-define "Aux" f4 (find-files *builddir* "boot.+" #t))
 (test-define "Aux" f0
              (find-files "/tmp"
                          "averyunlikelyfilenametobefoundintmp.innit" #f))
 (test-true "find-files 1" (null? f0))
 (test-false "find-files 2" (set-equal? f1 f2))
 (test-true "find-files 3" (contains? f1 f2))
 (test-false "find-files 4" (set-equal? f3 f4))
 (test-true "find-files 5" (contains? f3 f4))
 (test-true "find-files 6" (set-equal? f1 (list-dirent *builddir*)))
 (test/equal "find-files 7" (find-files *builddir* "spells" #f) '("spells"))
 (test/equal "find-files 8" (find-files *builddir* "spells$" #t) '("spells"))
 (test-true "find-files 9" (set-equal?
                            (map (lambda (e) (make-path "spells" e))
                                 (list-dirent (make-path *builddir* "spells")))
                            (find-files *builddir* "^spells/[^/]+$" #t)))

 (test-define "Aux" entries (list-dirent *builddir*))
 (test/equal "fold-dirent 1" (reverse (fold-dirent cons '() *builddir*)) entries)
 (test/eqv "fold-dirent 2"
           (fold-dirent (lambda (s n)
                               (+ n (string-length s))) 0 *builddir*)
           (string-length (apply string-append entries)))
 (test-true "fold-dirent 3"
            (set-equal? entries (map-dirent (lambda (e) e) *builddir*)))
 (test/eqv "fold-dirent 4"
           (let ((l1 0))
             (for-each-dirent (lambda (i) (set! l1 (+ 1 l1))) *builddir*)
             l1)
           (length entries))
 (test-true "fold-dirent 5"
            (set-equal? entries
                        (filter-dirent (lambda (i) #t) *builddir*)))
 (test-true "fold-dirent 6" (null? (filter-dirent (lambda (i) #f) *builddir*)))
 (test-true "fold-dirent 7"
            (null? (filter-dirent (lambda (i) #f) *builddir* #t)))
 (test-true "fold-dirent 8"
            (let ((res (list (car entries) (cadr entries))))
              (set-equal? res
                          (filter-dirent (lambda (e) (member e res))
                                         *builddir* #t))))
 (test-define "Aux" entries2 (find-files *builddir* ".+" #t))
 (test-true "fold-dirent 9"
            (set-equal? entries2 (fold-dirent cons '() *builddir* #t)))
 (test-true "fold-dirent 10"
            (set-equal? entries2
                        (map-dirent (lambda (e) e) *builddir* #t)))
 (test/eqv "fold-dirent 11"
           (let ((l1 0))
             (for-each-dirent
              (lambda (i) (set! l1 (+ 1 l1))) *builddir* #t) l1)
           (length entries2))
 (test-true "fold-dirent 12" (set-equal? entries2
                                         (filter-dirent
                                          (lambda (i) #t) *builddir* #t))))

;;; file-traversal.scm ends here
