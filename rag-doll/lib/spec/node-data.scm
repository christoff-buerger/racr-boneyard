#!r6rs

(library
 (rag-doll base rules-node-data)
 (export
  specify-rules-node-data)
 (import (rnrs) (racr core) (rag-doll util) (rnrs r5rs))


(define specify-rules-node-data
  (lambda (spec)
    (with-specification
     spec

     
 (ag-rule
  family-names
  (Node
   (lambda (n)
     (let* ((_res (list)) (_children (ast-child 'Node* n)) (_num (ast-num-children _children)))
       (cond ((> (ast-child 'nr n) 0)
             (set! _res (append _res (list (cons (ast-child 'nr n) (ast-child 'name n)))))))
       (ast-for-each-child
        (lambda (_i _child)
          (set! _res (append _res (att-value 'family-names _child))))
        _children)
       _res))))

     
     (ag-rule 
  referingnodes
  (Node
   (lambda (n)
     (let ((_list (list)) (_nr (ast-child 'nr n)))
       (let loop ((_n (att-value 'root-node n)))
         (let ((_refs (ast-child 'Ref* _n)) (_children (ast-child 'Node* _n)))
           (ast-for-each-child
            (lambda (_i _ref)
              (cond ((= _nr (ast-child 'nr (ast-child 'node _ref)))
                     (set! _list (append _list (list _n))))))
            _refs)
           (ast-for-each-child
            (lambda (_i _child)
              (loop _child))
            _children)))
       _list))))
 
 
  (ag-rule 
  all-references-list
  (Node
   (lambda (n)
     (let ((_list (list)))
       (let loop ((_n n))
         (let ((_refs (ast-child 'Ref* _n)) (_children (ast-child 'Node* _n)))
           (ast-for-each-child
            (lambda (_i _ref)
              (set! _list (append _list (list _ref))))
            _refs)
           (ast-for-each-child
            (lambda (_i _child)
              (loop _child))
            _children)))
       _list))))
 
 
 (ag-rule 
  get-references-list
  (Node
   (lambda (n _exclude)
     (if (null? _exclude)
         (att-value 'all-references-list n)
         (let ((_list (list)))
           (let loop ((_n n))
             (let ((_refs (ast-child 'Ref* _n)) (_children (ast-child 'Node* _n)))
               (ast-for-each-child
                (lambda (_i _ref)
                  (cond ((not (list-contains _exclude _ref))
                         (set! _list (append _list (list _ref))))))
                _refs)
               (ast-for-each-child
                (lambda (_i _child)
                  (loop _child))
                _children)))
           _list)))))
 
 
 (ag-rule
  rule
  (Node
   (lambda (n)
     (let ((_rname (att-value 'rule-name n)))
       (cond ((and _rname (string=? (symbol->string _rname) "") (att-value 'is-list-node? n))
              (let ((_prods (att-value 'children-productions (ast-parent (ast-parent n)))))
                (if (null? _prods)
                    '()
                    (let* ((_ch-prod (list-ref _prods (- (ast-child-index n) 1))))
                      (set! _rname (symbol->name _ch-prod)))))))
       (let ((_rule (specification->find-ast-rule (att-value 'user-spec n) _rname)))
         (if (not _rule)
             '()
             _rule))))))
 
 
 (ag-rule
  successors
  (Node
   (lambda (n)
     (let ((_res (list)))
       (cond ((> (ast-child 'nr n) 0)
              (let ((_parent (ast-parent (ast-parent n))) (_children (ast-child 'Node* n)))
                (cond ((> (ast-child 'nr _parent) 0) (set! _res (append _res (list (ast-child 'nr _parent))))))
                 (ast-for-each-child
                  (lambda (i n)
                    (set! _res (append _res (list (ast-child 'nr n)))))
                  (ast-child 'Node* n))
                 (ast-for-each-child
                  (lambda (i r)
                    (set! _res (append _res (list (ast-child 'nr (ast-child 'node r))))))
                  (ast-child 'Ref* n)))))
       _res))))

 
  (ag-rule
   reachable-nodes
   (Node
    (lambda (n)
      (let ((successors (att-value 'successors n)))
        (fold-left
         (lambda (result state)
           (let ((_succ-succs (att-value 'reachable-nodes (att-value 'get-node n state))))
             (list-union result _succ-succs)))
         successors
         successors)))
     (list)
    (lambda (r1 r2)
      (= (length r1) (length r2)))))

  
 (ag-rule
  mockup
  (Node
   (lambda (n)
     (if (null? (att-value 'rule n))
         '()
         (create-ast-mockup (att-value 'rule n))))))
 
 
 (ag-rule
  rule-clean-type
  (Node
   (lambda (n)
     (let* ((_type (ast-child 'type n)) (len (string-length _type)))
       (cond ((> len 2)
              (let ((_pre (substring _type 0 3)))
                (cond ((or (string=? _pre "T<=") (string=? _pre "T=>"))
                       (set! _type (substring _type 3)))
                      (else
                       (set! _pre (substring _type 0 2))
                       (cond ((or (string=? _pre "T<") (string=? _pre "T>"))
                              (set! _type (substring _type 2)))))))))
       _type))))
 
 
 (ag-rule
  rule-name
  (Node
   (lambda (n)
     (let ((_type (att-value 'rule-type n)))
       (if (not _type)
           #f
           (begin
             (set! _type (symbol->string _type))
             (let ((_pos (string-pos _type #\*)))
               (cond ((and (> _pos -1) (= _pos (- (string-length _type) 1)))
                      (set! _type (substring _type 0 _pos))))
               (if (string=? _type "")
                   #f
                   (string->symbol _type)))))))))
 
 
 (ag-rule
  rule-name-auto
  (Node
   (lambda (n)
     (let ((_rname (att-value 'rule-name n)))
       (if (not _rname)
           #f
           (if (string=? (symbol->string _rname) "")
               (cond ((and _rname (string=? (symbol->string _rname) "") (att-value 'is-list-node? n))
                      (let ((_prods (att-value 'children-productions (ast-parent (ast-parent n)))))
                        (if (null? _prods)
                            ""
                            (let* ((_ch-prod (list-ref _prods (- (ast-child-index n) 1))))
                              (symbol->name _ch-prod))))))
               _rname))))))
 
 
 (ag-rule
  rule-type
  (Node
   (lambda (n)
     (let ((_type (ast-child 'type n)))
       (if (string=? "" _type)
           #f
           (begin
             (let ((_pos (string-pos _type #\<)))
               (if (> _pos 0)
                   (string->symbol (substring _type (+ _pos 1)))
                   (string->symbol _type)))))))))
 
 
 (ag-rule
  children-productions
  (Node
   (lambda (n)
     (define _rule (att-value 'rule n))
     (if (null? _rule)
         '()
         (let ((_children (ast-child 'Node* n)) (_prods (ast-rule->production _rule)))
           (cond ((= (ast-child 'nr n) 0) (list))
                 ((= (ast-num-children _children) 0) '())
                 ((att-value 'is-list-node? n)
                  (let ((_ret (list)) (_t (list-ref _prods 0)))
                    (ast-for-each-child
                     (lambda (_i _child)
                       (set! _ret (append _ret (list _t))))
                     _children)
                    _ret))
                 (else
                  (if (null? _prods)
                      '()
                      (begin
                        (let* ((_sName (symbol->context-name (list-ref _prods 0))) (_nonTerminals '()) (_i 0))
                          (for-each
                           (lambda (_prod)
                             (cond ((> _i 0)
                                    (cond ((or (symbol->non-terminal? _prod) (symbol->kleene? _prod))
                                           (set! _nonTerminals (append _nonTerminals (list _prod))))))
                                   (else (set! _i 1))))
                           _prods)
                          _nonTerminals))))))))))
  
  
 (ag-rule
  all-children-productions
  (Node
   (lambda (n)
     (define _rule (att-value 'rule n))
     (if (null? _rule)
         '()
         (let ((_children (ast-child 'Node* n)) (_prods (ast-rule->production _rule)))
           (if (null? _prods)
               '()
               (begin
                 (let* ((_sName (symbol->context-name (list-ref _prods 0))) (_nonTerminals '()) (_i 0))
                   (for-each
                    (lambda (_prod)
                      (cond ((> _i 0)
                             (set! _nonTerminals (append _nonTerminals (list _prod))))
                            (else (set! _i 1))))
                    _prods)
                   _nonTerminals))))))))
  
  
 (ag-rule
  first-non-terminal-index
  (Node
   (lambda (n)
     (att-value 'non-terminal-index n 1))))

 
 (ag-rule
  non-terminal-index
  (Node
   (lambda (n _pos)
     (let ((_prods (att-value 'all-children-productions n)) (_i 1))
       (let ((_res (call/cc
                    (lambda (break)
                      (for-each
                       (lambda (_item)
                         (cond ((symbol->non-terminal? _item)
                                (if (= _pos 1)
                                    (break _i)
                                    (set! _pos (- _pos 1)))))
                         (set! _i (+ _i 1)))
                       _prods)
                      (break 0)))))
         _res)))))

 
 (ag-rule
  path-from-root
  (Node
   (lambda (n)
     (let ((_res '()))
       (let loop ((_n n))
         (let ((_parent (ast-parent (ast-parent _n))))
           (cond ((> (ast-child 'nr _parent) 0)
                  (let ((_num (att-value 'count-children _parent)) (_chN (ast-num-children (ast-child 'Node* _parent))))
                    (set! _res (append (list (cons "numCh" (if (or (= _num 0) (att-value 'is-list-node? _parent)) _chN _num)))
                                       (list (cons "gCh" (att-value 'matching-child-index _n)))
                                       _res )))
                  (loop _parent)))))
       _res))))
 
 
 (ag-rule
  path-to-root
  (Node
   (lambda (n)
     (let ((_res '()))
       (let loop ((_n n))
         (let ((_parent (ast-parent (ast-parent _n))))
           (cond ((> (ast-child 'nr _parent) 0)
                  (set! _res (append _res (list (cons "pCh" (att-value 'matching-child-index _n)) (cons "p" ""))))
                  (loop _parent)))))
       _res))))
 
 
 (ag-rule
  count-children
  (Node
   (lambda (n)
     (let ((_prods (att-value 'all-children-productions n)))
       (if (null? _prods)
           (let ((_chN (ast-num-children (ast-child 'Node* n))))
             (if (> _chN 0)
                 (- 0 _chN)
                 0))
           (length _prods))))))


 (ag-rule
  root-it-belongs-to
  (Node
   (lambda (n)
     (let loop ((_n n))
       (let ((_parent (ast-parent (ast-parent _n))))
         (if (= (ast-child 'nr _parent) 0)
             _n
             (loop _parent)))))))
 
 
 (ag-rule
  all-unique-foreign-references-with-names
  (Node
   (lambda (n)
     (let ((_refs (att-value 'all-foreign-references n)) (_unique (list)) (_res (list)) (_root-nr 0))
       (for-each
        (lambda (_ref)
          (set! _root-nr (ast-child 'nr (att-value 'root-it-belongs-to (ast-child 'node _ref))))
          (cond ((not (list-contains _unique _root-nr))
                 (set! _res (append _res (list (cons _ref (ast-child 'name _ref)))))
                 (set! _unique (append _unique (list _root-nr)))))
          )
        _refs)
       _res))))
 
 
 (ag-rule
  all-foreign-references
  (Node
   (lambda (n)
     (let ((_res (list)) (_refs (ast-child 'Ref* n)))
       (cond ((> (ast-num-children _refs) 0)
              (let ((_n-root-nr (ast-child 'nr (att-value 'root-it-belongs-to n))))
                (ast-for-each-child
                 (lambda (_i _ref)
                   (let ((_root1 (ast-child 'nr (att-value 'root-it-belongs-to (ast-child 'node _ref)))))
                     (cond ((not (= _root1 _n-root-nr))
                            (set! _res (append _res (list _ref)))))))
                 _refs))))
       (ast-for-each-child
        (lambda (_i _child)
          (let ((_list (att-value 'all-foreign-references _child)))
            (cond ((not (null? _list))
                   (set! _res (append _res _list))))))
        (ast-child 'Node* n))
       _res))))
 
 
   
     ))))
