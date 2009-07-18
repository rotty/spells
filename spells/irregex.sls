;; Copyright (c) 2009 Derick Eddington.  All rights reserved.  Licensed under an
;; MIT-style license.  My license is in the file named LICENSE from the original
;; collection this file is distributed with.  If this file is redistributed with
;; some other collection, my license must also be included.

#!r6rs

(library (spells irregex)
  (export
    irregex string->irregex sre->irregex irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-match-start-source irregex-match-start-index
    irregex-match-end-source irregex-match-end-index
    irregex-match-num-submatches irregex-match-substring
    irregex-match-index irregex-match-chunker
    irregex-search irregex-search/matches irregex-match
    irregex-replace irregex-replace/all irregex-fold irregex-fold/chunked
    irregex-search/chunked irregex-match/chunked
    make-irregex-chunker irregex-match-subchunk
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-num-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre maybe-string->sre)
  (import
    (rename (except (rnrs) error remove)
            (exists any) (for-all every) (remp remove))
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (spells include)
    (only (srfi :13) string-join))

  (define (error . args)
    (apply assertion-violation "(library (spells irregex))" args))

  (define string-intersperse string-join)
  
  (include-file ((spells private) irregex-r6rs))
  (include-file ((spells private) irregex-utils))
  
)
