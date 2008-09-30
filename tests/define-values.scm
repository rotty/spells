(testeez "basic"
  (test-true "true"
    (begin
      (define-values (a b c)
        (values 1 2 3))
      (list a b c))
    (list 1 2 3)))