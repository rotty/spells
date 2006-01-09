(testeez "object application"
  (test/equal "null" ((object list)) '())
  (test/equal "list"  ((object list) 1 2 3)  '(1 2 3)))

(testeez "operations"
  (test/equal "default" ((operation list) 1 2 3) '(1 2 3)))

(let* ((op (operation #f))
       (obj (object #f ((op a b c) (list a b c)))))
  (testeez "objects and operations"
    (test/equal "operate" (op obj 1 2) (list obj 1 2))))
