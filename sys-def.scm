;;@ A portability library. It offers a single interface to
;; functionality commonly present, but not standardized in various
;; Scheme implementations.
(define-system spells

  (r6rs-libs "libraries")

  (conjure
   (import (rnrs)
           (conjure cc)
           (conjure hostinfo)
           (conjure dsl))

   (task cc (cc-conf))
   
   (task (configure
          (produce '((("spells" "foreign") "config.sls")
                     <= (("libraries" "foreign") "config.sls.in")))
          (fetchers (cc-fetcher 'cc)
                    (hostinfo-fetcher))))))

;; arch-tag: 8717975c-90a7-4442-87e2-e2ba9efecca3
