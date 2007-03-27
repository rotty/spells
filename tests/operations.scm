(testeez "object application"
  (test/equal "null" ((object list)) '())
  (test/equal "list"  ((object list) 1 2 3)  '(1 2 3)))

(testeez "operations"
  (test/equal "default" ((operation list) 1 2 3) '(1 2 3)))

(let* ((op (operation #f))
       (obj (object #f ((op a b c) (list a b c)))))
  (testeez "objects and operations"
    (test/equal "operate" (op obj 1 2) (list obj 1 2))))

(let* ((op1 (operation #f))
       (op2 (operation #f))
       (o1 (object #f ((op1 self a b c) (list a b c))))
       (o2 (object #f ((op2 self a b c) (vector a b c))))
       (joined-obj (join o1 o2)))
  (testeez "join"
    (test/equal "op1" (op1 joined-obj 1 2 3) (list 1 2 3))
    (test/equal "op2" (op2 joined-obj 1 2 3) (vector 1 2 3))))
