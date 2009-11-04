;;@ A portability library. It offers a single interface to
;; functionality commonly present, but not standardized in various
;; Scheme implementations.
(package (spells (0))

  (depends
   (srfi))
  
  (libraries
   ("spells" . libs))

  (conjure
   (import (rnrs)
           (conjure cc)
           (conjure hostinfo)
           (conjure dsl))

   (task cc (cc-conf))
   
   (task (configure
          (produce '((("spells" "foreign") "config.sls")
                     <= (("spells" "foreign") "config.sls.in")))
          (fetchers (cc-fetcher 'cc)
                    (hostinfo-fetcher))))))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
