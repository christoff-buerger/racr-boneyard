#!r6rs


(library
 (rag-doll base model)
 (export
  (rename (add-new-node                  model-add-new-node))
  (rename (add-node                      model-add-node))
  (rename (add-reference-for-curr-node   model-add-reference-for-curr-node))
  (rename (auto-fill-children            model-auto-fill-children))
  (rename (create-inner-spec             model-create-inner-spec))
  (rename (create-new-node               model-create-new-node))
  (rename (clear-tree                    model-clear-tree))
  (rename (find-node                     model-find-node))
  (rename (get-all-names                 model-get-all-names))
  (rename (get-all-nodes                 model-get-all-nodes))
  (rename (get-copy-node                 model-get-copy-node))
  (rename (get-curr-node                 model-get-curr-node))
  (rename (get-curr-ref                  model-get-curr-ref))
  (rename (get-cut-node                  model-get-cut-node))
  (rename (get-last-node-nr              model-get-last-node-nr))
  (rename (get-main-node                 model-get-main-node))
  (rename (get-names-list                model-get-names-list))
  (rename (get-node-children             model-get-node-children))
  (rename (get-node-links                model-get-node-links))
  (rename (get-node-name                 model-get-node-name))
  (rename (get-node-references           model-get-node-references))
  (rename (get-node-references-as-list   model-get-node-references-as-list))
  (rename (get-node-to-cut               model-get-node-to-cut))
  (rename (get-node-type                 model-get-node-type))
  (rename (get-ref-name                  model-get-ref-name))
  (rename (get-root-node                 model-get-root-node))
  (rename (get-rule-data                 model-get-rule-data))
  (rename (get-rule-terminals            model-get-rule-terminals))
  (rename (get-user-spec                 model-get-user-spec))
  (rename (init-tree                     model-init-tree))
  (rename (modify-node-status            model-modify-node-status))
  (rename (reset-node-status             model-reset-node-status))
  (rename (save-new-user-spec            model-save-new-user-spec))
  (rename (serialize-node                model-serialize-node))
  (rename (set-copy-node                 model-set-copy-node))
  (rename (set-curr-node                 model-set-curr-node))
  (rename (set-curr-ref                  model-set-curr-ref))
  (rename (set-cut-node                  model-set-cut-node))
  (rename (set-main-node                 model-set-main-node))
  (rename (unset-main-node               model-unset-main-node))
  (rename (set-node-type                 model-set-node-type))
  (rename (set-node-name                 model-set-node-name))
  (rename (set-ref-name                  model-set-ref-name))
  (rename (set-root-node-as-curr         model-set-root-node-as-curr))
  (rename (update-area-size              model-update-area-size)))
 (import
  (rnrs)
  (racr core)
  (rag-doll base types))
 
 
 ; ---------- variables ----------
; (define tree '())
; (define user-spec '())
; (define inner-spec '())
; (define last-nr 0)

 
 ; ---------- functions ----------
 (define save-new-user-spec
   (lambda (_cfg _new-spec)
     (rewrite-terminal 'userspec (rag-doll-config-tree _cfg) _new-spec)))
 
 
 (define get-user-spec
   (lambda (_cfg)
     (ast-child 'userspec (rag-doll-config-tree _cfg))))
 
 
 (define get-last-node-nr
   (lambda (_cfg)
     (rag-doll-config-last-nr _cfg)))
 
 
 (define create-new-node
   (lambda (_cfg)
     (create-ast (rag-doll-config-inner-spec _cfg) 'Node (list (create-ast-list '()) 0 "" 0 '() (create-ast-list '())))))
 
 
 (define create-inner-spec
   (lambda ()
     (create-specification)))
 
 
 (define init-tree
   (lambda (_ispec _uspec _area-w _area-h)
     (let ((_root (create-ast _ispec 'Node (list (create-ast-list '()) 0 "" 0 '() (create-ast-list '()) ""))))
       (create-ast _ispec 'Tree (list _root _area-w _area-h _uspec _root '() '() '() '())))))
 
 
 (define get-all-names
   (lambda (_cfg)
     (let loop ((_l (get-all-nodes _cfg)))
       (if (null? _l)
           (list "")
           (append
            (list (ast-child 'name (car _l)))
            (loop (cdr _l)))))))
 

 (define get-all-nodes
   (lambda (_cfg)
     (let ((_res '()))
       (let loop ((_node (ast-child 'Node (rag-doll-config-tree _cfg))))
         (cond ((> (ast-child 'nr _node) 0)
                (set! _res (append _res (list _node)))))
         (let ((_children (ast-child 'Node* _node)))
           (cond ((> (ast-num-children _children) 0)
                  (ast-for-each-child
                   (lambda (i _child)
                     (loop _child))
                   _children)))))
       _res)))
 

 (define add-new-node
   (lambda (_cfg _parent _pos)
     (rag-doll-config-last-nr-set! _cfg (+ (rag-doll-config-last-nr _cfg) 1))
     (add-node _cfg _parent _pos (list (rag-doll-config-last-nr _cfg) ""))))


 (define auto-fill-children
   (lambda (_cfg _node _types)
     (for-each
      (lambda (_item)
        (let ((_new-node (add-new-node _cfg _node 1)))
          (set-node-type _new-node _item)))
      (reverse _types))))
 
 
 (define add-node
   (lambda (_cfg _parent _pos _data)
     (let* ((_node (create-ast (rag-doll-config-inner-spec _cfg) 'Node (list (create-ast-list '()) (list-ref _data 0) (list-ref _data 1) 0 '() (create-ast-list '()) ""))) (_children (ast-child 'Node* _parent)))
       (let ((_n (ast-num-children _children)))
          (cond ((and (> _n 0) (or (< _pos 1) (> _pos _n))) (set! _pos (+ _n 1))))
          (cond ((< _pos 1) (set! _pos 1)))
          (rewrite-insert _children _pos _node))
        _node)))


  (define get-root-node
    (lambda (_cfg)
      (ast-child 'Node (rag-doll-config-tree _cfg))))

  
  (define get-curr-node  (lambda (_cfg)        (ast-child 'currnode (rag-doll-config-tree _cfg))))
  (define set-curr-node  (lambda (_cfg _node)  (rewrite-terminal 'currnode (rag-doll-config-tree _cfg) _node)))
  
  (define get-cut-node   (lambda (_cfg)        (ast-child 'cutnode (rag-doll-config-tree _cfg))))
  (define set-cut-node   (lambda (_cfg _node)  (rewrite-terminal 'cutnode (rag-doll-config-tree _cfg) _node)))
  
  (define get-copy-node  (lambda (_cfg)        (ast-child 'copynode (rag-doll-config-tree _cfg))))
  (define set-copy-node  (lambda (_cfg _node)  (rewrite-terminal 'copynode (rag-doll-config-tree _cfg) _node)))
  
  (define get-curr-ref   (lambda (_cfg)        (ast-child 'currref (rag-doll-config-tree _cfg))))
  (define set-curr-ref   (lambda (_cfg _ref)   (rewrite-terminal 'currref (rag-doll-config-tree _cfg) _ref)))
  
  
  (define get-node-references
    (lambda (_node)
      (ast-child 'Ref* _node)))
  
  
  (define get-node-references-as-list
    (lambda (_node)
      (let* ((_refs (ast-child 'Ref* _node)) (_max (ast-num-children _refs)))
        (let ((_res (list)))
          (ast-for-each-child
           (lambda (_i _item)
             (set! _res (append _res (list _item))))
           _refs)
          _res))))
  
  
  (define get-node-children
    (lambda (_node)
      (ast-child 'Node* _node)))
  
  
  (define add-reference-for-curr-node
    (lambda (_cfg _ref-to)
      (let* ((_node (get-curr-node _cfg)) (_refs (get-node-references _node)) (_ref-to-nr (ast-child 'nr _ref-to)) (_ref #f))
        (call/cc
         (lambda (break)
           (ast-for-each-child
            (lambda (_i _ref)
              (cond ((= (ast-child 'nr (ast-child 'node _ref)) _ref-to-nr) (break #t))))
            _refs)
           (set! _ref (create-ast (rag-doll-config-inner-spec _cfg) 'Ref (list _ref-to "")))
           (rewrite-insert _refs 1 _ref)))
        _ref)))

  
  (define clear-tree
    (lambda (_cfg)
      (let* ((_children (get-node-children (get-root-node _cfg))))
        (let loop ((_n (ast-num-children _children)))
          (cond ((> _n 0)
                 (rewrite-delete (ast-child _n _children))
                 (loop (- _n 1))))))
      (rag-doll-config-last-nr-set! _cfg  0)
      (set-root-node-as-curr _cfg)))

  
  (define set-root-node-as-curr
    (lambda (_cfg)
      (set-curr-node _cfg (get-root-node _cfg))))
  
  
  (define reset-node-status
    (lambda (_node)
      (rewrite-terminal 'status _node 0)))
  
  
  (define find-node
    (lambda (_cfg _nr)
      (call/cc
       (lambda (break)
         (let loop ((_n (get-root-node _cfg)))
           (cond ((= (ast-child 'nr _n) _nr) (break _n)))
           (let ((_children (get-node-children _n)))
             (ast-find-child
              (lambda (i _child)
                (loop _child))
              _children)))
         (break '())))))
  
  
  (define get-node-links
    (lambda (_n _r)
      (att-value 'children-links _n _r)))
  
  
 (define update-area-size
   (lambda (_cfg _w _h)
     (rewrite-terminal 'areawidth (rag-doll-config-tree _cfg) _w)
     (rewrite-terminal 'areaheight (rag-doll-config-tree _cfg) _h)))
 
 
 (define modify-node-status
   (lambda (_node _val)
     (rewrite-terminal 'status _node (+ (ast-child 'status _node) _val))))
 
 
 (define serialize-node
   (lambda (_node _recirsiv _with_refs)
     (let ((_list '()) (_i 0) (_parent-nr 0) (_parent-child-i 0) (_data-list '()))
       (let loop ((_n _node) (_children (get-node-children _node)) (_parent-i 0))
         (set! _i (+ _i 1))
         (cond ((> (ast-child 'nr _n) 0)
                (set! _parent-nr (ast-child 'nr (ast-parent (ast-parent _n))))
                (set! _parent-child-i (ast-child-index (ast-parent _n)))))
         (set! _data-list (list (ast-child 'nr _n)
                                (ast-child 'type _n)
                                _parent-nr
                                _parent-child-i
                                _i            ; for copying to clipboard
                                _parent-i
                                (ast-child 'name _n)))
         (cond (_with_refs
                (let ((_refs (get-node-references _n)) (_refs-data '()))
                  (ast-for-each-child
                   (lambda (_i _ref)
                     (set! _refs-data (append _refs-data (list (list (ast-child 'nr (ast-child 'node _ref)) (ast-child 'name _ref))))))
                   _refs)
                  (set! _data-list (append _data-list (list _refs-data))))))
         (set! _list (append _list (list _data-list)))
         (set! _parent-i _i)
         (let loop2 ((_n (ast-num-children _children)))
           (cond ((> _n 0)
                  (let ((_child (ast-child _n _children)))
                    (loop _child (get-node-children _child) _parent-i))
                  (loop2 (- _n 1)))))
         (lambda (_ind _child)
           _children))
       _list)))
 
 
 (define get-main-node   (lambda (_cfg)        (att-value 'get-main-node (rag-doll-config-tree _cfg))))
 (define set-main-node   (lambda (_cfg _node)  (rewrite-terminal 'mainnode (rag-doll-config-tree _cfg) _node)))
 (define unset-main-node (lambda (_cfg)        (rewrite-terminal 'mainnode (rag-doll-config-tree _cfg) '())))
 
 (define get-node-type  (lambda (_node)       (ast-child 'type _node)))
 (define set-node-type  (lambda (_node _type) (rewrite-terminal 'type _node _type)))
 
 (define get-node-name  (lambda (_node)       (ast-child 'name _node)))
 (define set-node-name  (lambda (_node _name) (rewrite-terminal 'name _node _name)))
 
 (define get-ref-name   (lambda (_ref)        (ast-child 'name _ref)))
 (define set-ref-name   (lambda (_ref _name)  (rewrite-terminal 'name _ref _name)))
 
 
 (define get-node-to-cut
   (lambda (_cfg)
     (ast-child 'cutnode (rag-doll-config-tree _cfg))))

 
 (define get-names-list
   (lambda (_cfg)
     (att-value 'family-names (get-root-node _cfg))))
 
 
 (define get-rule-data
   (lambda (_rule)
     (let ((_prods (ast-rule->production _rule)) (_names '()))
          (set! _names (let loop ((_list (cdr _prods)))
                         (if (null? _list)
                             '()
                             (let* ((_item  (car _list))
                                    (_oname (string-append (symbol->string (symbol->name _item)) (if (symbol->kleene? _item) "*" "")))
                                    (_cname (symbol->string (symbol->context-name _item))))
                               (append
                                (list (if (string=? _oname _cname)
                                          _oname
                                          (string-append _oname "<" _cname)))
                                (loop (cdr _list)))))))
       (append (list (symbol->name (car _prods))) (list (ast-rule->symbolic-representation _rule))))))
 
 
 (define get-rule-terminals
   (lambda (_rule)
     (let ((_prods (ast-rule->production _rule)) (_names '()))
          (let loop ((_list (cdr _prods)))
            (if (null? _list)
                '()
                (append
                 (if (not (symbol->non-terminal? (car _list)))
                     '()
                     (let ((_item  (car _list)))
                       (list (list (symbol->string (symbol->name _item)) (if (symbol->kleene? _item) #t #f)))))
                 (loop (cdr _list)))))))))
 
