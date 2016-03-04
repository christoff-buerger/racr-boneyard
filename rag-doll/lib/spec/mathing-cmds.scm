#!r6rs

(library
 (rag-doll base rules-mathing-cmds)
 (export
  specify-rules-mathing-cmds)
 (import (rnrs) (racr core) (rag-doll util) (rnrs r5rs))


(define specify-rules-mathing-cmds
  (lambda (spec)
    (with-specification
     spec
     
     (ag-rule
      matching-child-index
      (Node
       (lambda (n)
         (let* ((_parent (ast-parent (ast-parent n))) (_prods (att-value 'all-children-productions _parent)))
           (cond ((att-value 'is-list-node? _parent)
                  (ast-child-index n))
                 ((null? _prods)
                  (- 0 (ast-child-index n)))
                 (else
                  (if (not (att-value 'rule-name n))
                      (att-value 'non-terminal-index _parent (ast-child-index n))
                      (let ((_i 1) (_rname (att-value 'rule-name n)))
                        (cond ((not _rname)
                               (let ((_rpos (ast-child-index n))) ; get position of the node and find the non-terminal (e.g. index = 2 then find 2nd non-terminal)
                                 (let loop ((_list _prods))
                                   (if (null? _list)
                                       -1
                                       (begin
                                         (cond ((symbol->non-terminal? (car _list))
                                                (if (= _rpos 0)
                                                    _i
                                                    (begin
                                                      (set! _i (+ _i 1))
                                                      (set! _rpos (- _rpos 1))
                                                      (loop (cdr _list)))))
                                               (else
                                                (loop (cdr _list)))))))))
                              (else
                               (let loop ((_list _prods))
                                 (if (null? _list)
                                     -1
                                     (begin
                                       (if (eq? _rname (symbol->name (car _list)))
                                           _i
                                           (begin
                                             (set! _i (+ _i 1))
                                             (loop (cdr _list)))))))))))))))))
     
     
     (ag-rule
      reference-mathing-path
      (Ref
       (lambda (r)
         (let ((_from-node (ast-parent (ast-parent r))) (_refering-node (ast-child 'node r)))
           (append
            (list (cons "gR" (ast-child 'nr (att-value 'root-it-belongs-to _from-node))))
            (att-value 'path-from-root _from-node)
            (list (cons "r" (ast-child 'name r)))
            (att-value 'path-to-root _refering-node)
            (list (cons "cR" (ast-child 'nr (att-value 'root-it-belongs-to _refering-node)))))))))
     
     
     (ag-rule
      find-references-mathing-paths
      (Tree
       (lambda (t _skip-refs)
         (let ((_ret '()) (_refs (att-value 'get-references-list (ast-child 'Node t) _skip-refs)))
           (for-each
            (lambda (_ref)
              (set! _ret (append _ret (list (att-value 'reference-mathing-path _ref)))))
            _refs)
           _ret))))
     
     
     (ag-rule
      node-structure-mathing-path
      (Node
       (lambda (n)
         (let ((_path '()))
           (let ((_type (ast-child 'type n)) (_name (ast-child 'name n)))
             (cond ((not (string=? _type ""))
                    (set! _path (append (list (cons "chType" _type)) _path))))
             (cond ((not (string=? _name ""))
                    (set! _path (append (list (cons "sName" _name)) _path)))))
           (let* ((_children (ast-child 'Node* n)) (_n (att-value 'count-children n)) (_chN (ast-num-children _children)))
             (cond ((> _chN 0)
                    (set! _path (append (list (cons "numCh" (if (or (= _n 0) (att-value 'is-list-node? n)) _chN _n))) _path))
                    (ast-for-each-child
                     (lambda (_i _child)
                       (set! _path (append (list (cons "p" ""))
                                           (att-value 'node-structure-mathing-path _child)
                                           (list (cons "gCh" (att-value 'matching-child-index _child))) _path)))
                     _children))))
           _path))))))))
