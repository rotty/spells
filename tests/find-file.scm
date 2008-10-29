(testeez "find-file"
  (test-false "nonexisting" (find-file ".abracadabra.khgafd" (library-search-paths)))
  (test-true "existing" (and (find-file '((spells)) (library-search-paths)) #t)))
