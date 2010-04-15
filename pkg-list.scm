;;@ A portability library. It offers a single interface to
;; functionality commonly present, but not standardized in various
;; Scheme implementations.
(package (spells (0))

  (depends
   (srfi)
   (wak-irregex)
   (wak-foof-loop)
   (wak-fmt)
   (wak-trc-testing))

  (libraries
   (exclude ("spells" "foreign")
            ("spells" "foreign.sls"))
   ("spells" "private")
   ("spells" . sls)))

(package (spells-foreign (0))
  
  (depends
   (srfi)
   (spells)
   (wak-foof-loop)
   (conjure))

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
         (foreign-conjure-tasks)
       
         (task install
           (ordinary
            (depends 'configure)
            (proc (lambda (self)
                    (let ((product (pathname-join ((self 'project) 'product-dir)
                                                  config-pathname)))
                      (agent 'install-file
                             'libraries
                             (->namestring config-pathname)
                             (->namestring product))))))))))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list conjure-dsl)
;; End:
