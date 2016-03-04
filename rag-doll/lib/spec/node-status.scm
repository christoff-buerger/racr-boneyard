#!r6rs

(library
 (rag-doll base rules-node-status)
 (export
  specify-rules-node-status)
 (import (rnrs) (racr core) (rag-doll util) (rnrs r5rs))


(define specify-rules-node-status
  (lambda (spec)
    (with-specification
     spec
     
     (ag-rule
      is-main-to-cut?
      (Node
       (lambda (n)
         (and (att-value 'is-main? n) (att-value 'is-to-cut? n)))))
     
     
     (ag-rule
      is-to-cut?
      
      ((Tree Node)
       (lambda (t)
         #f))
      
      ((Node Node*)
       (lambda (n)
         (or
          (eq? (ast-child 'status n) 2)  ;cutted
          (eq? (ast-child 'status n) 3)  ;cutted-main
          (eq? (ast-child 'status n) 12) ;selected-cutted
          (eq? (ast-child 'status n) 13) ;selected-cutted-main
          (att-value 'is-to-cut? (ast-parent n))))))
     
     
     (ag-rule
      reaches-all-start-nodes?
      (Tree
       (lambda (t _list)
         (let ((_children (ast-child 'Node* (ast-child 'Node t))))
           (call/cc
            (lambda (break)
              (ast-for-each-child
               (lambda (_i _child)
                 (let ((_nr (ast-child 'nr _child)))
                   (cond ((not (list-contains _list _nr))
                          (break _nr)))))
               _children)
              (break 0)))))))
     
     
     (ag-rule
      reaches-all?
      (Node
       (lambda (n)
         (att-value 'reaches-all-start-nodes? n (append (list (ast-child 'nr n)) (att-value 'reachable-nodes n))))))
     
     
     (ag-rule
      is-parent?
      (Node
       (lambda (n fn)
         (let ((_findnr (ast-child 'nr fn)))
           (call/cc
            (lambda (break)
              (let loop ((_node n))
                (ast-for-each-child
                 (lambda (_i _child)
                   (if (= (ast-child 'nr _child) _findnr)
                       (break #t)
                       (loop _child)))
                 (ast-child 'Node* _node)))
              (break #f)))))))
     
     
     (ag-rule
      is-direct-parent?
      (Node
       (lambda (n fn)
         (= (ast-child 'nr n) (ast-child 'nr (ast-parent (ast-parent fn)))))))
     
     
     (ag-rule
      is-selected
      (Node
       (lambda (n)
         (if (> (ast-child 'status n) 9) #t #f))))
     
     
     (ag-rule
      is-main?
      (Node
       (lambda (n)
         (= (att-value 'main-node-nr n) (ast-child 'nr n)))))
     
     
     (ag-rule
      is-list-node?
      (Node
       (lambda (n)
         (let* ((_type (ast-child 'type n)) (_len (- (string-length _type) 1)))
           (and (> _len -1) (string=? "*" (substring _type _len)))))))
     
     
     (ag-rule
      is-type-correct?
      
      (Tree
       (lambda (t)
         (att-value 'is-type-correct? (ast-child 'Node t) 0)))
      
      (Ref
       (lambda (r)
         (not (string=? (ast-child 'name r) ""))))
      
      (Node
       (lambda (n)
         (let ((_ret (call/cc
                      (lambda (break)
                        (let ((_nr (ast-child 'nr n)) (_children (ast-child 'Node* n)) (_child-type-empty? "") (_type (att-value 'rule-name-auto n)) (_mockup '()) (_child-type '()) (_child-ret '()) (_prods '()) (_curr-prod '()))
                          (let ((_refs (ast-child 'Ref* n)))
                            (ast-for-each-child
                             (lambda (_i _ref)
                               (cond ((not (att-value 'is-type-correct? _ref))
                                      (break (cons 11 (cons _nr (ast-child 'nr (ast-child 'node _ref))))))))
                             _refs))
                          (if (or (= _nr 0) (not _type))
                              (ast-for-each-child
                               (lambda (_i _child)
                                 (let ((_ret (att-value 'is-type-correct? _child)))
                                   (cond ((> (car _ret) 0) (break _ret)))))
                               _children)
                              (begin
                                (cond ((att-value 'is-list-node? n)
                                       (let ((_can-be-empty? #t))
                                         (ast-for-each-child
                                          (lambda (_i _child)
                                            (set! _child-type (att-value 'rule-name-auto _child))
                                            (set! _child-type-empty? (if (symbol? _child-type)
                                                                         (string=? (symbol->string _child-type)  "")
                                                                         #t))
                                            (if (or (not _child-type) _child-type-empty?)
                                                (if (and (not _child-type) (att-value 'rule-name _child))
                                                    (break (cons 0 (ast-child 'nr _child)))
                                                    (if (att-value 'is-list-node? _child)
                                                        (break (cons 7 (ast-child 'nr _child)))
                                                        (if _can-be-empty?
                                                            (begin
                                                              (cond ((not _child-type-empty?) (set! _can-be-empty? #f)))
                                                              (let ((_ret (att-value 'is-type-correct? _child)))
                                                                (cond ((> (car _ret) 0)
                                                                       (break _ret)))))
                                                            (break (cons 8 (ast-child 'nr _child))))))
                                                (if (and (string? _type) (string=? _type "") (not (string=? "" (if (string? (att-value 'rule-name _child)) (att-value 'rule-name _child) (symbol->string (att-value 'rule-name _child))))))
                                                    (break (cons 0 (ast-child 'nr _child)))
                                                    (if (ast-subtype? (att-value 'mockup _child) _type)
                                                        (let ((_ret (att-value 'is-type-correct? _child)))
                                                          (cond ((> (car _ret) 0)
                                                                 (break _ret))))
                                                        (break (cons 9 (ast-child 'nr _child)))))))
                                          _children)))
                                      (else
                                       (if (and (not (att-value 'rule-name n)) (> (ast-num-children n) 0))
                                           (break (cons 10 _nr))
                                           (begin
                                             (set! _prods (att-value 'children-productions n))
                                             (if (not (= (length _prods) (ast-num-children _children)))
                                                 (break (cons 3 (cons _nr (length _prods))))
                                                 (begin
                                                   (ast-for-each-child
                                                    (lambda (_i _child)
                                                      (set! _curr-prod (list-ref _prods (- _i 1)))
                                                      (set! _nr (ast-child 'nr _child))
                                                      (if (not (eq? (att-value 'is-list-node? _child) (eq? #\* (symbol->kleene? _curr-prod))))
                                                          (if (and (eq? #\* (symbol->kleene? _curr-prod)) (not (att-value 'rule-name _child)))
                                                              (break (cons 0 _nr))
                                                              (break (cons 4 _nr)))
                                                          (begin
                                                            (set! _mockup (att-value 'mockup _child))
                                                            (if (null? _mockup)
                                                                (if (att-value 'rule-name _child)
                                                                    (break (cons 5 _nr))
                                                                    (break (cons -1 _nr)))
                                                                (if (not (ast-subtype? _mockup (symbol->name _curr-prod)))
                                                                    (break (cons 6 _nr))
                                                                    (begin
                                                                      (set! _child-ret (att-value 'is-type-correct? _child))
                                                                      (cond ((> (car _child-ret) 0)
                                                                             (break _child-ret)))))))))
                                                    _children)))))))))
                          (break (cons 0 _nr)))))))
           _ret))))))))
