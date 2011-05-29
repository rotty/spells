;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.


;;; Main libraries

(package (spells (0))

  (depends
   (srfi)
   (wak-irregex)
   (wak-foof-loop)
   (wak-fmt)
   (wak-trc-testing))

  (synopsis "portability and utility library")
  (description
   "A portability library. It offers a single interface to"
   "functionality commonly present, but not standardized, in various"
   "Scheme implementations."
   ""
   "Spells currently offers:"
   " - A filesystem interface"
   " - A pathname facility"
   " - An interface to OS processes"
   " - A simple interface to TCP sockets"
   " - Weak cells"
   " - An implementation of the zipper data structure"
   " - Extensible vectors")
  (homepage "http://rotty.yi.org/software/spells/")
  
  (stexidoc "docs/spells.scm")

  (libraries
   (exclude ("spells" "foreign")
            ("spells" "foreign.sls"))
   ("spells" "private")
   ("spells" . sls)))


;;; Foreign-function interface

(package (spells-foreign (0))
  
  (depends
   (srfi)
   (spells)
   (wak-foof-loop)
   (conjure))

  (synopsis "foreign function interface to C")
  (description
   "An foreign function interface that allows Scheme code"
   "to interact with code (libraries) written in C.")
  (homepage "http://rotty.yi.org/software/spells/")
  
  (libraries
   ("spells" "foreign" . sls)
   ("spells" "foreign.sls"))

  (conjure
   (import (rnrs base)
           (spells foreign conjure))

   (foreign-conjure-tasks))
  
  (installation-hook ((needs-source? . #t))
    (import (rnrs)
            (spells pathname)
            (spells foreign conjure)
            (conjure dsl)
            (conjure dorodango))

    (make-conjure-hook
     (lambda (agent)
       (let ((config-pathname (->pathname
                               '(("spells" "foreign") "config.sls"))))
         (task install
           (ordinary
            (depends 'configure)
            (proc (lambda (self)
                    (let ((product (pathname-join ((self 'project) 'product-dir)
                                                  config-pathname)))
                      (agent 'install-file
                             'libraries
                             (->namestring config-pathname)
                             (->namestring product)))))))
         
         (foreign-conjure-tasks))))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list conjure-dsl)
;; End:
