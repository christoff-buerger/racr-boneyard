#!r6rs

(library
 (rag-doll base)
 (export
  (rename (add-new-node                    rag-doll_add-new-node))
  (rename (add-reference                   rag-doll_add-reference))
  (rename (auto-fill-children              rag-doll_auto-fill-children))
  (rename (call-qsort-string               rag-doll_call-qsort-string))
  (rename (can-create-mathing-commands?    rag-doll_can-create-mathing-commands?))
  (rename (clear-tree                      rag-doll_clear-tree))
  (rename (create-mathing-commands         rag-doll_create-mathing-commands))
  (rename (delete-curr-node                rag-doll_delete-curr-node))
  (rename (delete-curr-ref                 rag-doll_delete-curr-ref))
  (rename (delete-type-from-list           rag-doll_delete-type-from-list))
  (rename (fill-tree                       rag-doll_fill-tree))
  (rename (find-node                       rag-doll_find-node))
  (rename (get-all-nodes                   rag-doll_get-all-nodes))
  (rename (get-area-h                      rag-doll_get-area-h))
  (rename (get-area-w                      rag-doll_get-area-w))
  (rename (get-bitmap-data                 rag-doll_get-bitmap-data))
  (rename (get-curr-node                   rag-doll_get-curr-node))
  (rename (get-curr-node-children-numbers  rag-doll_get-curr-node-children-numbers))
  (rename (get-curr-node-name              rag-doll_get-curr-node-name))
  (rename (get-curr-node-nr                rag-doll_get-curr-node-nr))
  (rename (get-curr-node-type              rag-doll_get-curr-node-type))
  (rename (get-curr-node-type-for-ast-info rag-doll_get-curr-node-type-for-ast-info))
  (rename (get-curr-ref                    rag-doll_get-curr-ref))
  (rename (get-names-list                  rag-doll_get-names-list))
  (rename (get-node-children               rag-doll_get-node-children))
  (rename (get-node-children-as-list       rag-doll_get-node-children-as-list))
  (rename (get-node-converted-references   rag-doll_get-node-converted-references))
  (rename (get-node-links                  rag-doll_get-node-links))
  (rename (get-name-of-sel-obj             rag-doll_get-name-of-sel-obj))
  (rename (get-node-range-restriction      rag-doll_get-node-range-restriction))
  (rename (get-node-ray                    rag-doll_get-node-ray))
  (rename (get-node-reference-on-pos       rag-doll_get-node-reference-on-pos))
  (rename (get-node-references             rag-doll_get-node-references))
  (rename (get-node-references-as-list     rag-doll_get-node-references-as-list))
  (rename (get-node-x                      rag-doll_get-node-x))
  (rename (get-node-x-space                rag-doll_get-node-x-space))
  (rename (get-node-y                      rag-doll_get-node-y))
  (rename (get-rewrite-conds               rag-doll_get-rewrite-conds))
  (rename (get-root-node                   rag-doll_get-root-node))
  (rename (get-rule-details                rag-doll_get-rule-details))
  (rename (get-rule-position               rag-doll_get-rule-position))
  (rename (get-rules-names                 rag-doll_get-rules-names))
  (rename (get-text-of-sel-obj             rag-doll_get-text-of-sel-obj))
  (rename (init                            rag-doll_init))
  (rename (init-inner-spec                 rag-doll_init-inner-spec))
  (rename (is-any-node-set-to-cut?         rag-doll_is-any-node-set-to-cut?))
  (rename (is-ast-child?                   rag-doll_is-ast-child?))
  (rename (is-copy-node-set?               rag-doll_is-copy-node-set?))
  (rename (is-curr-node-list-node?         rag-doll_is-curr-node-list-node?))
  (rename (is-curr-node-set?               rag-doll_is-curr-node-set?))
  (rename (is-curr-ref-set?                rag-doll_is-curr-ref-set?))
  (rename (is-cut-node-set?                rag-doll_is-cut-node-set?))
  (rename (is-node-main?                   rag-doll_is-node-main?))
  (rename (is-node-main-to-cut?            rag-doll_is-node-main-to-cut?))
  (rename (is-node-to-cut?                 rag-doll_is-node-to-cut?))
  (rename (is-tree-empty?                  rag-doll_is-tree-empty?))
  (rename (load-state                      rag-doll_load-state))
  (rename (move-node                       rag-doll_move-node))
  (rename (paste-cutted-node               rag-doll_paste-cutted-node))
  (rename (save-new-name                   rag-doll_save-new-name))
  (rename (save-new-type                   rag-doll_save-new-type))
  (rename (save-state                      rag-doll_save-state))
  (rename (serialize-copy-node             rag-doll_serialize-copy-node))
  (rename (set-curr-as-main                rag-doll_set-curr-as-main))
  (rename (set-curr-node                   rag-doll_set-curr-node))
  (rename (set-curr-node-by-nr             rag-doll_set-curr-node-by-nr))
  (rename (set-curr-node-to-copy           rag-doll_set-curr-node-to-copy))
  (rename (set-curr-node-to-cut            rag-doll_set-curr-node-to-cut))
  (rename (set-default-data                rag-doll_set-default-data))
  (rename (set-node-as-curr                rag-doll_set-node-as-curr))
  (rename (set-ref-as-curr                 rag-doll_set-ref-as-curr))
  (rename (set-rewrite-conds               rag-doll_set-rewrite-conds))
  (rename (unset-curr-node                 rag-doll_unset-curr-node))
  (rename (unset-curr-ref                  rag-doll_unset-curr-ref))
  (rename (update-area-size                rag-doll_update-area-size)))
 (import
  (rnrs)
  (racr core)
  (racr testing)
  (rag-doll util)
  (rag-doll base types)
  (rag-doll base ast)
  (rag-doll base model)
  (rag-doll base rules-print)
  (rag-doll base rules-node-status)
  (rag-doll base rules-node-data)
  (rag-doll base rules-data)
  (rag-doll base rules-mathing-cmds))
 
 
 ; ---------- variables ----------
 (define ast-rules-list '())
 (define ast-rules-details-list '())
 (define default-ref-name "ref-value")
 (define rewrite-conds "")

 
 ; ---------- functions ----------
 (define set-rewrite-conds
   (lambda (_val)
     (set! rewrite-conds _val)
     #t))
 
 
 (define get-rewrite-conds
   (lambda ()
     rewrite-conds))
 
 
 (define set-default-data
   (lambda (_data)
     (cond ((and (list? _data) (> (length _data) 0))
            (for-each
             (lambda (_item)
               (cond ((string=? (car _item) "def_ref_name") (set! default-ref-name (cdr _item)))))
             _data)))))
 
 
 (define is-tree-empty?
   (lambda (_cfg)
     (= 0 (ast-num-children (ast-child 'Node* (model-get-root-node _cfg))))))
 
 
 (define init-inner-spec
   (lambda ()
     (let ((_inner-spec (model-create-inner-spec)))
       (specify-ast _inner-spec)
       (compile-ast-specifications _inner-spec 'Tree)
       (specify-rules-print _inner-spec)
       (specify-rules-node-status _inner-spec)
       (specify-rules-node-data _inner-spec)
       (specify-rules-data _inner-spec)
       (specify-rules-mathing-cmds _inner-spec)
       (compile-ag-specifications _inner-spec)
       _inner-spec)))
 
 
 (define init
   (lambda (_uspec _area-w _area-h)
     (let* ((_ispec (init-inner-spec)) (_tree (model-init-tree _ispec _uspec _area-w _area-h)))
       (let ((_cfg (make-rag-doll-config _ispec _tree 0)))
         (model-set-root-node-as-curr _cfg)
         (analize-user-spec _cfg)
         _cfg))))
 
 
 
 (define analize-user-spec
   (lambda (_cfg)
     (let* ((_spec (model-get-user-spec _cfg)) (_rules (specification->ast-rules _spec)))
       (set! ast-rules-details-list '())
       (for-each
        (lambda (_item)
          (set! ast-rules-details-list (append ast-rules-details-list (list (get-rule-full-details _cfg _item)))))
        _rules))))
 
 
 (define get-rules-names
   (lambda ()
     (let ((_list (list)))
       (for-each 
        (lambda (_item)
          (set! _list (append _list (list (symbol->string (car (list-ref _item 0)))))))
        ast-rules-details-list)
       (qsort-string-no-list _list))))
 
 
 (define get-rule-details
   (lambda (_rule-name)
     (let loop ((_list ast-rules-details-list))
       (if (null? _list)
           '()
           (if (equal? (list-ref (car (car _list)) 0) (if (string? _rule-name) (string->symbol _rule-name) _rule-name))
               (car _list)
               (loop (cdr _list)))))))
 
 
 (define get-rule-position
   (lambda (_rule-name)
     (if (symbol? _rule-name) (symbol->string _rule-name) _rule-name)
     (let loop ((_l (get-rules-names)) (_i 0))
       (if (null? _l)
           #f
           (cond ((string=? (car _l) _rule-name)
                  _i)
                 (else
                  (loop (cdr _l) (+ _i 1))))))))
 
 
 (define select-curr-node
   (lambda (_cfg)
     (let ((_curr-node (model-get-curr-node _cfg)))
       (cond ((and (> (ast-child 'nr _curr-node) 0) (not (att-value 'is-selected _curr-node)))
              (rewrite-terminal 'status _curr-node (+ (ast-child 'status _curr-node) 10)))))))
 
 
 (define unselect-curr-node
   (lambda (_cfg)
     (let ((_curr-node (model-get-curr-node _cfg)))
       (cond ((and (> (ast-child 'nr _curr-node) 0) (att-value 'is-selected _curr-node))
              (rewrite-terminal 'status _curr-node (- (ast-child 'status _curr-node) 10)))))))

 
  (define uncut-node-to-cut
    (lambda (_cfg)
      (let ((_cut-node (model-get-cut-node _cfg)))
        (cond ((and (not (null? _cut-node)) (att-value 'is-to-cut? _cut-node))
               (rewrite-terminal 'status _cut-node (- (ast-child 'status _cut-node) 2)))))))

  
  (define get-curr-node
    (lambda (_cfg)
      (model-get-curr-node _cfg)))
  
  
  (define set-curr-node
    (lambda (_cfg _node)
      (unset-curr-ref _cfg)
      (unselect-curr-node _cfg)
      (cond ((ast-node? _node)
             (model-set-curr-node _cfg _node)
             (select-curr-node _cfg)
             #t)
            (else
             (set-root-as-curr-node _cfg)
             #f))))
  
  
  (define unset-curr-node
    (lambda (_cfg)
      (let ((_curr-node (model-get-curr-node _cfg)))
        (cond ((> (ast-child 'nr _curr-node) 0)
               (let ((_stat (ast-child 'status _curr-node)))
                 (cond ((> _stat 9)
                        (model-modify-node-status _curr-node -10))))
               (set-root-as-curr-node _cfg)
               #t)
              (else
               #f)))))
  
  
  (define unset-curr-ref
    (lambda (_cfg)
      (model-set-curr-ref _cfg '())))
  
  
  (define set-root-as-curr-node
    (lambda (_cfg)
      (model-set-root-node-as-curr _cfg)))
  
  
  (define set-curr-node-by-nr
    (lambda (_cfg _nr)
      (set-curr-node _cfg (model-find-node _cfg _nr))))
  
  
  (define type-correct?
    (lambda (_cfg)
      (att-value 'is-type-correct? (get-root-node _cfg))))
  
    
  (define save-new-type
    (lambda (_cfg _type)
      (define (prepare-type _type)
        (let ((_len (string-length _type)))
          (cond ((> _len 2)
                 (cond ((or (string=? "T<=" (substring _type 0 3)) (string=? "T=>" (substring _type 0 3)))
                        (set! _type (substring _type 3))
                        (set! _len (- _len 3)))
                       ((or (string=? "T<" (substring _type 0 2)) (string=? "T>" (substring _type 0 2)))
                        (set! _type (substring _type 2))
                        (set! _len (- _len 2))))))
          (cond ((= (string-pos _type #\*) (- _len 1))
                 (set! _type (substring _type 0 (- _len 1))))))
        _type)
      
      (call/cc
       (lambda (break)
         (cond ((is-curr-node-set? _cfg)
                (let ((_list-node? #f) (_len (string-length _type)))
                  (cond ((> _len 0)
                         (set! _list-node? (= (string-pos _type #\*) (- _len 1)))))
                  (cond ((or (string=? _type "") (string=? _type "*") (list-contains (get-rules-names) (prepare-type _type)))
                         (model-set-node-type (model-get-curr-node _cfg) _type)
                         (break 0))
                        (else
                         (break -2)))))
               ((is-curr-ref-set? _cfg)
                (model-set-ref-name (model-get-curr-ref _cfg) _type)
                (break 1)))
         (break -1)))))
  
  
  (define save-new-name
    (lambda (_cfg _val)
      (define (contains? _list _val)
        (if (null? _list)
            #f
            (if (string=? (cdr (car _list)) _val)
                (car (car _list))
                (contains? (cdr _list) _val))))
      
      (cond ((is-curr-node-set? _cfg)
             (let ((_res (and (not (string=? _val "")) (contains? (get-names-list _cfg) _val))))
               (cond ((not _res)
                      (model-set-node-name (model-get-curr-node _cfg) _val)
                      0)
                     (else
                      _res))))
            (else -1))))
  
  
  (define move-node
    (lambda (_cfg _side)
      (let ((_curr-node (model-get-curr-node _cfg)))
        (cond ((> (ast-child 'nr _curr-node) 0)
               (let ((_parent (ast-parent _curr-node)))
                 (let* ((_i (ast-child-index _curr-node)) (_n (ast-num-children _parent)))
                   (rewrite-delete _curr-node)
                   (if (= _side -1)
                       (begin
                         (set! _i (- _i 1))
                         (cond ((< _i 1) (set! _i _n))))
                       (begin
                         (set! _i (+ _i 1))
                         (cond ((> _i _n) (set! _i 1)))))
                   (rewrite-insert _parent _i _curr-node))))))))
  
  
  (define delete-curr-node
    (lambda (_cfg _all)
      (let ((_curr-node (model-get-curr-node _cfg)))
        (cond (_all
               (let loop ((_n _curr-node))
                 (delete-pointing-references _cfg _n)
                 (let ((_children (model-get-node-children _n)))
                   (ast-for-each-child
                    (lambda (_i _child)
                      (loop _child))
                    _children)))
               (rewrite-delete _curr-node))
              (else
               (delete-pointing-references _cfg _curr-node)
               (let* ((_i (ast-child-index _curr-node)) (_list (ast-parent _curr-node)) (_children (model-get-node-children _curr-node)) (_n (ast-num-children _children)))
                 (rewrite-delete _curr-node)
                 (let loop ()
                   (cond ((> _n  0)
                          (let ((_child (ast-child _n _children)))
                            (rewrite-insert _list _i (rewrite-delete _child))
                            (set! _n (- _n 1))
                            (loop)))))))))))
  
  
  (define delete-pointing-references
    (lambda (_cfg n)
      (let ((_nrs (att-value 'referingnodes n)) (_node-nr (ast-child 'nr n)))
        (for-each
         (lambda (_node)
           (let* ((_refs (model-get-node-references _node)) (_refs_n (ast-num-children _refs)))
             (let loop ((_i 1) (_ref (ast-child 1 _refs)))
               (cond ((<= _i _refs_n)
                      (set! _i (+ _i 1))
                      (cond ((= (ast-child 'nr (ast-child 'node _ref)) _node-nr)
                             (rewrite-delete _ref))
                            (else
                             (loop _i (ast-child _i _refs)))))))))
         _nrs))))
  
  
  (define get-rule-full-details
    (lambda (_cfg _rule)
      (cond ((string? _rule) (set! _rule (string->symbol _rule))))
      (cond ((symbol? _rule) (set! _rule (specification->find-ast-rule (model-get-user-spec _cfg) _rule))))
      (if (null? _rule)
          '()
          (let ((_inh-data (list)) (_fields '()) (_prods '()))
            (set! _inh-data (append
                             (list (model-get-rule-data _rule))
                             (let loop ((_parent (ast-rule->supertype? _rule)))
                               (if (not _parent)
                                   '()
                                   (append
                                    (list (model-get-rule-data _parent))
                                    (loop (ast-rule->supertype? _parent)))))))
            _inh-data))))
  
  
  (define save-state
    (lambda (_cfg _fpath _types)
      (cond ((file-exists? _fpath) (delete-file _fpath)))
      (let* ((_fh (open-output-file _fpath)) (_spec (model-get-user-spec _cfg)) (_rules (specification->ast-rules _spec)) (_main-node (model-get-main-node _cfg)))
        (write (specification->start-symbol _spec) _fh)
        (newline _fh)
        (write (if (not _main-node) 0 (ast-child 'nr _main-node)) _fh)
        (newline _fh)
        (for-each
         (lambda (_rule)
           (write (ast-rule->symbolic-representation _rule) _fh)
           (newline _fh))
         (reverse _rules))
        (newline _fh)
        (cond ((> (string-length rewrite-conds) 0)
               (write rewrite-conds _fh)))
        (newline _fh)
        (newline _fh)
        (write (model-serialize-node (model-get-root-node _cfg) #t #t) _fh)
        (newline _fh)
        (newline _fh)
        (write _types _fh)
        (newline _fh)
        (newline _fh)
        (write (get-names-list _cfg) _fh)
        (close-output-port _fh))))
      
  
  (define load-state
    (lambda (_cfg _fpath)
      (define _spec-symbol '())
      (define _spec-main-node '())
      (define _new-spec '())
      (define _nodes '())
      (define _types '())
      (define (read-line _fh)
        (let ((_ch (read-char _fh)))
          (if (or (eof-object? _ch) (equal? _ch #\newline))
            ""
            (string-append (list->string (list _ch)) (read-line _fh)))))
      
      (define (spec-comp-err-handl thunk)
        (call/cc
         (lambda (k)
           (with-exception-handler
            (lambda (x) (if (racr-exception? x) (k #f) (raise x)))
            thunk))))

      (let ((p (open-input-file _fpath)) (_spec-err 0))
        (set! _spec-symbol (read-line p))  ;read start symbol
        (if (string=? _spec-symbol "")
            (cons "err" -1)
            (begin
              (set! _spec-symbol (string->symbol _spec-symbol))
              (set! _spec-main-node (read-line p))  ;read spec phase
              (if (string=? _spec-main-node "")
                  (cons "err" -2)
                  (begin
                    (set! _spec-main-node (string->number _spec-main-node))
                    (set! _new-spec (create-specification))  ;create new specyfication
                    (guard (ex 
                            ((racr-exception? ex) (set! _spec-err 1))
                            (else                 (set! _spec-err 2)))
                           (with-specification
                            _new-spec
                            (let loop () ; recreate rules
                              (let ((_line (read-line p)))
                                (cond ((not (string=? _line ""))
                                       (ast-rule (string->symbol _line))
                                       (loop)))))
                            (compile-ast-specifications _spec-symbol)
                            (compile-ag-specifications)))
                    (if (> _spec-err 0)
                        (cons "err" -3)
                        (begin
                          (model-save-new-user-spec _cfg _new-spec)
                          (analize-user-spec _cfg)
                          (let ((_txt (read-line p)))
                            (set! rewrite-conds (if (> (string-length _txt) 2)
                                                    (substring _txt 1 (- (string-length _txt) 1))
                                                    _txt)))
                          (read-line p)                      ;empty line
                          (set! _nodes (read-line p))        ;read nodes data
                          (read-line p)                      ;empty line
                          (set! _types (read-line p))        ;read types data
                          (close-input-port p)
                          (guard (ex
                                  ((racr-exception? ex) (set! _spec-err 1))
                                  (else                 (set! _spec-err 2)))
                                 (let* ((_sip (open-string-input-port _nodes)) (_nodes (get-datum _sip)))
                                   (load-tree _cfg (cdr _nodes))    ;not using fill-tree due to a separated lists of names and references and node numbers problems
                                   (load-references _cfg (cdr _nodes))
                                   (close-input-port _sip))
                                 (let* ((_sip (open-string-input-port _types)))
                                   (set! _types (get-datum _sip))
                                   (close-input-port _sip))
                                 (cond ((> _spec-main-node 0)
                                        (model-set-main-node _cfg (model-find-node _cfg _spec-main-node)))))
                          (if (> _spec-err 0)
                              (cons "err" -4)
                              (cons "ok" _types)))))))))))
      
  
  (define load-tree
    (lambda (_cfg _nodes)
      (clear-tree _cfg)
      (let ((_node '()) (_max-nr 0))
        (for-each
         (lambda (_node-data)
           (set! _node (model-add-node _cfg (model-find-node _cfg (list-ref _node-data 2)) (list-ref _node-data 3) (list (list-ref _node-data 0) (list-ref _node-data 1))))
           (set-curr-node _cfg _node)
           (save-new-name _cfg (list-ref _node-data 6))
           (set! _max-nr (max _max-nr (list-ref _node-data 0))))
         _nodes)
        (rag-doll-config-last-nr-set! _cfg (+ _max-nr 1)))))
  
  
  (define load-references
    (lambda (_cfg _nodes)
      (let ((_refs '()) (_ref '()))
        (for-each
         (lambda (_node-data)
           (set-curr-node _cfg (model-find-node _cfg (list-ref _node-data 0)))
           (set! _refs (list-ref _node-data 7))
           (for-each
            (lambda (_ref-data)
              (let ((_n (model-find-node _cfg (car _ref-data))) (_ref '()))
                (cond ((not (null? _n))
                       (set! _ref (add-reference _cfg _n))
                       (model-set-ref-name _ref (list-ref _ref-data 1))))))
            _refs))
         _nodes))))
  
  

  (define serialize-copy-node
    (lambda (_cfg)
      (let ((_copy-node (model-get-copy-node _cfg)))
        (cond ((> (ast-child 'nr _copy-node) 0)
               (model-serialize-node _copy-node #t #f))
              (else
               '())))))
  
  
  (define set-curr-node-to-copy
    (lambda (_cfg)
      (let ((_curr-node (model-get-curr-node _cfg)))
        (cond ((> (ast-child 'nr _curr-node) 0)
               (uncut-node-to-cut _cfg)
               (model-set-cut-node _cfg '())
               (model-set-copy-node _cfg _curr-node)
               #t)
              (else
               #f)))))
  
  
  (define set-curr-node-to-cut
    (lambda (_cfg)
      (let ((_cut-node (model-get-cut-node _cfg)) (_curr-node (model-get-curr-node _cfg)))
        (cond ((ast-node? _cut-node)
               (rewrite-terminal 'status _cut-node (- (ast-child 'status _cut-node) 2))))
        (model-set-copy-node _cfg '())
        (cond ((> (ast-child 'nr _curr-node) 0)
               (model-set-cut-node _cfg _curr-node)
               (set! _cut-node _curr-node)
               (rewrite-terminal 'status _cut-node (+ (ast-child 'status _cut-node) 2))
               #t)
              (else
               (model-set-cut-node _cfg '())
               #f)))))

  
  (define clear-cut
    (lambda (_cfg)
      (let ((_cut-node (model-get-cut-node _cfg)))
        (cond ((not (null? _cut-node))
               (let ((_stat (ast-child 'status _cut-node)))
                 (cond ((or (= _stat 12) (= _stat 2))
                        (rewrite-terminal 'status _cut-node (- _stat 2))))))))))
  
  
  (define fill-tree
    (lambda (_cfg _list _pos _sel-first _save-names _switch-root)
      (let ((_i 0) (_curr-nr (ast-child 'nr (model-get-curr-node _cfg))) (_orig-curr-node (model-get-curr-node _cfg)) (_node '()) (last-nr (model-get-last-node-nr _cfg)) (copy_to (ast-child 'nr (get-curr-node _cfg))))
        (for-each
         (lambda (_node-data)
           (cond ((> _i 0)
                  (set-curr-node _cfg (model-find-node _cfg (+ last-nr (list-ref _node-data 5))))
                  (set! _pos (list-ref _node-data 3))))
           (set! _node (model-add-node _cfg (model-get-curr-node _cfg) _pos (list (+ last-nr (list-ref _node-data 4)) (list-ref _node-data 1))))
           (cond (_save-names
                  (set-curr-node _cfg _node)
                  (save-new-name _cfg (list-ref _node-data 6))))
           (set! _i (+ _i 1)))
         _list)
        (rag-doll-config-last-nr-set! _cfg (+ last-nr (length _list)))
        (if _sel-first
            (let ((_children (model-get-node-children (model-get-root-node _cfg))))
              (cond ((> (ast-num-children _children) 0)
                     (set-curr-node _cfg (ast-child 1 _children)))))
            (set-curr-node _cfg (model-find-node _cfg _curr-nr))))))
  
  
  (define is-copy-node-set?  (lambda (_cfg)  (ast-node? (model-get-copy-node _cfg))))
  (define is-cut-node-set?   (lambda (_cfg)  (ast-node? (model-get-cut-node _cfg))))
  (define is-curr-node-set?  (lambda (_cfg)  (> (ast-child 'nr (model-get-curr-node _cfg)) 0)))
  (define is-curr-ref-set?   (lambda (_cfg)  (ast-node? (model-get-curr-ref _cfg))))
  
  
  (define paste-cutted-node
    (lambda (_cfg _pos)
      (let ((_cut-node (model-get-cut-node _cfg)) (_curr-node (model-get-curr-node _cfg)))
        (cond ((not (null? _cut-node))
               (let* ((_cut-nr (ast-child 'nr _cut-node)) (paste-to-child? (call/cc
                                                                            (lambda (break)
                                                                              (let loop ((_n _curr-node) (_nr (ast-child 'nr _curr-node)))
                                                                                (cond ((> _nr 0)
                                                                                       (if (= _nr _cut-nr)
                                                                                           (break #f)
                                                                                           (begin
                                                                                             (set! _n (ast-parent (ast-parent _n)))
                                                                                             (loop _n (ast-child 'nr _n))))))
                                                                                (break #t))))))
                 (cond (paste-to-child?
                        (rewrite-delete _cut-node)
                        (model-modify-node-status _cut-node -2)
                        (let* ((_children (model-get-node-children _curr-node)) (_n (+ 1 (ast-num-children _children))))
                          (cond ((or (> _pos _n) (= _pos -1)) (set! _pos _n)))
                          (rewrite-insert _children _pos _cut-node))
                        (model-set-cut-node _cfg '())
                        0)
                       (else
                        1))))
              (else
               2)))))
  
  
  (define get-names-list
    (lambda (_cfg)
      (qsort-cons (model-get-names-list _cfg))))
  
  
  (define call-qsort-string
    (lambda (l)
      (qsort-string l)))
  
  
  (define set-ref-as-curr
    (lambda (_cfg _ref)
      (unset-curr-ref _cfg)
      (unset-curr-node _cfg)
      (cond ((not (null? _ref))
             (model-set-curr-ref _cfg _ref)
             #t)
            (else
             #f))))
  
  
  (define set-node-as-curr
    (lambda (_cfg _node)
      (unset-curr-ref _cfg)
      (unset-curr-node _cfg)
      (cond ((not (null? _node))
             (model-set-curr-node _cfg _node)
             #t)
            (else
             #f))))
  
  
  (define get-curr-ref
    (lambda (_cfg)
      (model-get-curr-ref _cfg)))
  
  
  (define add-reference
    (lambda (_cfg _refto)
      (if (> (ast-child 'nr (model-get-curr-node _cfg)) 0)
          (model-add-reference-for-curr-node _cfg _refto)
          #f)))
  
  
  (define get-bitmap-data
    (lambda (_node)
      (att-value 'bitmap-params _node)))
  
  
  (define is-node-to-cut?      (lambda (_node) (att-value 'is-to-cut? _node)))
  (define is-node-main?        (lambda (_node) (att-value 'is-main? _node)))
  (define is-node-main-to-cut? (lambda (_node) (att-value 'is-main-to-cut? _node)))
  
  
  (define is-any-node-set-to-cut?
    (lambda (_cfg)
      (not (null? (model-get-node-to-cut _cfg)))))
  
  
  (define get-node-x-space
    (lambda (_node)
      (att-value 'x-space _node)))
  
  
  (define get-node-ray
    (lambda (_node)
      (att-value 'node-ray _node)))
  
  
  (define get-node-range-restriction
    (lambda (_node)
      (att-value 'range-restriction _node)))
  
  
  (define get-node-x
    (lambda (_node)
      (att-value 'x _node)))
  
  
  (define get-node-y
    (lambda (_node)
      (att-value 'y _node)))
  
  
  (define get-area-w
    (lambda (_cfg)
      (att-value 'area-w (rag-doll-config-tree _cfg))))
  
  
  (define get-area-h
    (lambda (_cfg)
      (att-value 'area-h (rag-doll-config-tree _cfg))))
  
  
  (define get-node-children-as-list
    (lambda (_node)
      (define (loop _list _n _pos)
        (append
         (list (ast-child _pos _list))
         (if (>= _pos _n)
             (list)
             (loop _list _n (+ _pos 1)))))
      (let ((_children (model-get-node-children _node)))
        (if (= (ast-num-children _children) 0)
            (list)
            (loop _children (ast-num-children _children) 1)))))
      
      
  (define get-node-converted-references
    (lambda (_node)
      (att-value 'references _node '())))
  
  
  (define get-curr-node-children-numbers
    (lambda (_cfg)
      (define (loop _list)
        (append
         (list (ast-child 'nr (car _list)))
         (if (null? (cdr _list))
             (list)
             (loop (cdr _list)))))
      (let ((_children (get-node-children-as-list (model-get-curr-node _cfg))))
        (if (null? _children)
            (list)
            (loop _children)))))
  
  
  (define get-curr-node-type
    (lambda (_cfg)
      (ast-child 'type (model-get-curr-node _cfg))))
  

  (define get-curr-node-type-for-ast-info
    (lambda (_cfg)
      (let ((_node (model-get-curr-node _cfg)))
        (if (att-value 'is-list-node? _node)
            "*"
            (ast-child 'type _node)))))
  
  
  (define is-curr-node-list-node?
    (lambda (_cfg)
      (att-value 'is-list-node? (model-get-curr-node _cfg))))
  
  
  (define get-curr-node-nr
    (lambda (_cfg)
      (ast-child 'nr (model-get-curr-node _cfg))))
  
  
  (define get-curr-node-name
    (lambda (_cfg)
      (ast-child 'name (model-get-curr-node _cfg))))
  
  
  (define set-curr-as-main
    (lambda (_cfg)
      (model-set-main-node _cfg (ast-child 'currnode (rag-doll-config-tree _cfg)))))
  
  
  (define delete-curr-ref
    (lambda (_cfg)
      (let ((_ref (ast-child 'currref (rag-doll-config-tree _cfg))))
        (unset-curr-ref _cfg)
        (rewrite-delete _ref))))
  
  
  (define get-text-of-sel-obj
    (lambda (_cfg)
      (cond ((is-curr-node-set? _cfg)  (model-get-node-type (model-get-curr-node _cfg)))
            ((is-curr-ref-set? _cfg)   (model-get-ref-name (model-get-curr-ref _cfg)))
            (else                      ""))))
  
  
  (define get-name-of-sel-obj
    (lambda (_cfg)
      (cond ((is-curr-node-set? _cfg)  (model-get-node-name (model-get-curr-node _cfg)))
            (else                      ""))))
  
  
  (define is-ast-child?
    (lambda (_node)
      (ast-node? _node)))
  
  
  (define delete-type-from-list
    (lambda (_cfg _type)
      (let loop ((_n (model-get-root-node _cfg)))
        (cond ((string=? (model-get-node-type _n) _type)
               (model-set-node-type _n "")))
        (let ((_children (model-get-node-children _n)))
          (ast-for-each-child
           (lambda (_i _child)
             (loop _child))
           _children))
        #t)))
  
  
  (define get-node-reference-on-pos
    (lambda (_node _pos)
      (ast-child _pos (model-get-node-references _node))))
  
  
  (define get-root-nodes-numbers
    (lambda (_cfg)
      (let ((_ret (list)) (_children (ast-child 'Node* (model-get-root-node _cfg))))
        (ast-for-each-child
         (lambda (_i _child)
           (set! _ret (append _ret (list (ast-child 'nr _child)))))
         _children)
        _ret)))
  
  
  (define get-all-sub-references
    (lambda (_cfg _root)
      (let ((_res (list)) (_root-nr (ast-child 'nr _root)))
        (let loop ((_node _root))
          (let ((_children (ast-child 'Node* _node)) (_refs (ast-child 'Ref* _node)))
            (cond ((> (ast-num-children _refs) 0)
                   (ast-for-each-child
                    (lambda (_i _ref)
                      (cond ((not (= _root-nr (ast-child 'nr (att-value 'graph-root-node (ast-child 'node _ref)))))
                             (set! _res (append _res (list _ref))))))
                    _refs)))
            (ast-for-each-child
             (lambda (_i _child)
               (loop _child))
             _children)))
        _res)))
  

  (define can-create-mathing-commands?
    (lambda (_cfg)
      (if (null? (model-get-user-spec _cfg))
          (cons -1 0)
          (if (= (ast-num-children (model-get-node-children (model-get-root-node _cfg))) 0)
              (cons -2 0)
              (let ((_main-node (model-get-main-node _cfg)))
                (if (not _main-node)
                    (cons -3 0)
                    (if (att-value 'is-list-node? _main-node)
                        (cons -7 0)
                        (if (string=? (ast-child 'type _main-node) "")
                            (cons -6 0)
                            (let ((_res (type-correct? _cfg)))
                              (if (> (car _res) 0)
                                  (cons -4 _res)
                                  (let ((_ra (att-value 'reaches-all? _main-node)))
                                    (if (> _ra 0)
                                        (cons -5 _ra)
                                        (cons 0 (cons 0 0))))))))))))))


  (define get-node-children-types
    (lambda (_cfg _node)
      (cond ((att-value 'is-list-node? _node)  (cons -1 '()))
            ((> (ast-num-children (ast-child 'Node* _node)) 0)  (cons -2 '()))
            (else
             (let ((_type (ast-child 'type _node)))
               (if (string=? _type "")
                   (cons -3 '())
                   (begin
                     (let ((_cnt 0) (_types (list)) (_prods (ast-rule->production (specification->find-ast-rule (model-get-user-spec _cfg) (string->symbol _type)))))
                       (for-each
                        (lambda (_item)
                          (cond ((symbol->non-terminal? _item)
                                 (set! _cnt (+ _cnt 1))
                                 (set! _types (append _types (list (string-append
                                                                    (symbol->string (symbol->name _item))
                                                                    (if (symbol->kleene? _item) "*" ""))))))))
                        (cdr _prods))
                       (if (= _cnt 0)
                           (cons -4 '())
                           (cons 0 _types))))))))))
  
  
  (define auto-fill-children
    (lambda (_cfg)
      (let* ((_curr-node (model-get-curr-node _cfg)) (_ret (get-node-children-types _cfg _curr-node)) (_ret-code (car _ret)))
        (cond ((= _ret-code 0)
               (model-auto-fill-children _cfg _curr-node (cdr _ret))
               0)
              (else
               _ret-code)))))
  
  
  (define get-all-nodes               (lambda (_cfg)        (model-get-all-nodes _cfg)))
  (define add-new-node                (lambda (_cfg _pos)   (model-add-new-node _cfg (model-get-curr-node _cfg) _pos)))
  (define get-root-node               (lambda (_cfg)        (model-get-root-node _cfg)))
  (define get-node-children           (lambda (_node)       (model-get-node-children _node)))
  (define get-node-references         (lambda (_node)       (model-get-node-references _node)))
  (define get-node-references-as-list (lambda (_node)       (model-get-node-references-as-list _node)))
  (define clear-tree                  (lambda (_cfg)        (model-clear-tree _cfg)))
  (define find-node                   (lambda (_cfg _nr)    (model-find-node _cfg _nr)))
  (define get-node-links              (lambda (_n _r)       (model-get-node-links _n _r)))
  (define update-area-size            (lambda (_cfg _w _h)  (model-update-area-size _cfg _w _h)))
  

  (define create-mathing-commands
    (lambda (_cfg)
      (define _left-roots-nums-list '())
      (define _roots-paths '())
      (define _refs-paths '())
      (define _graphs-paths '())
      (define _used-refs '())
      
      (define (searched-for-root? _root)
        (list-contains _left-roots-nums-list (ast-child 'nr _root)))
      
      (define (set-root-as-found _root)
        (set! _left-roots-nums-list (list-delete-number _left-roots-nums-list (ast-child 'nr _root))))
      
      (define (find-unique-roots _node _path)
        (let* ((_root (att-value 'root-it-belongs-to _node)) (_root-nr (ast-child 'nr _root)))
          (cond ((searched-for-root? _root) ;found one of the searched roots
                 (set-root-as-found _root)
                 (set! _roots-paths (append _roots-paths (list (append _path (att-value 'path-to-root _node) (list (cons "sR" _root-nr))))))
                 (set! _path (list (cons "gR" _root-nr)))
                 (let ((_urefs (att-value 'all-unique-foreign-references-with-names _root)) (_ref '()) (_ref-name ""))
                   (for-each
                    (lambda (_uref)
                      (cond ((> (length _left-roots-nums-list) 0)
                             (set! _ref (car _uref))
                             (set! _ref-name (if (string=? "" (cdr _uref))
                                                 default-ref-name
                                                 (cdr _uref)))
                             (set! _used-refs (append _used-refs (list _ref)))
                             (find-unique-roots (ast-child 'node _ref) (append
                                                                        _path
                                                                        (att-value 'path-from-root (ast-parent (ast-parent _ref)))
                                                                        (list (cons "r" _ref-name)))))))
                    _urefs)))
                (else
                 (cond ((> (length _used-refs) 0)
                        (set! _used-refs (cdr (reverse _used-refs)))))))))
      
      (define (get-all-root-nums)
        (set! _left-roots-nums-list (att-value 'get-all-root-nodes-nums (rag-doll-config-tree _cfg))))
        
      (define (find-unused-references-paths)
        (set! _refs-paths (append _refs-paths (att-value 'find-references-mathing-paths (rag-doll-config-tree _cfg) _used-refs))))
      
      (define (find-graphs-correction-paths)
        (let ((_res '()) (_roots (ast-child 'Node* (model-get-root-node _cfg))))
          (ast-for-each-child
           (lambda (_i _root)
             (let ((_children (ast-child 'Node* _root)) (_struct-path (att-value 'node-structure-mathing-path _root)))
               (set! _graphs-paths (append _graphs-paths (list (append
                                                                (list (cons "gR" (ast-child 'nr _root)))
                                                                (if (null? _struct-path) '() (reverse _struct-path))))))))
           _roots)))
      
      (define cmd-cnt 0)
      (define (path->string _path)
        (string-append
         ""
         (let loop ((_list _path))
           (if (null? _list)
               ""
               (string-append
                ""
                (let loop2 ((_list2 (car _list)) (_cnt cmd-cnt))
                  (if (null? _list2)
                      (begin
                        (set! cmd-cnt _cnt)
                        "")
                      (string-append
                       "\n    (cons "
                       (string-append "'" (caar _list2))
                       " "
                       (let ((_val (cdar _list2)))
                         (if (number? _val)
                             (number->string _val)
                             (if (string=? "" _val)
                                 "\"\""
                                 (string-append "\"" _val "\""))))
                       ")"
                       (loop2 (cdr _list2) (+ _cnt 1)))))
                (loop (cdr _list)))))))
      
      (define (create-bindings _cfg)
        (let ((_str (string-append ""
                     (let loop ((_l (model-get-all-names _cfg)))
                       (if (null? _l)
                           ""
                           (let ((_item (car _l)))
                             (string-append
                              (if (string=? _item "")
                                  ""
                                  (string-append " (" _item " (cdr (assoc '" _item " _res)))"))
                              (loop (cdr _l)))))))))
          _str))
      
      
      (let ((_main-node (model-get-main-node _cfg)))
        (get-all-root-nums)
        (find-unique-roots (model-get-main-node _cfg) '())
        (find-graphs-correction-paths)
        (find-unused-references-paths)
        
        (let* ((_rewrite-conds? (not (string=? rewrite-conds "")))
               (_bindings-string (create-bindings _cfg))
               (_bindings? (not (string=? _bindings-string ""))))
          (cond (_rewrite-conds?
                 (string-append " (" (att-value 'rule-clean-type _main-node) "\n"
                                "  (lambda (n)\n"
                                "   (let ((_res (rgd_match-pattern n (list "
                                (path->string _roots-paths)
                                (path->string _graphs-paths)
                                (if (null? _refs-paths) "" (path->string _refs-paths))
                                "))))\n"
                                "    (and\n"
                                "     _res\n"
                                (cond (_bindings?
                                       (string-append "     (let ("
                                                      _bindings-string
                                                      ")\n"
                                                      "      (if " rewrite-conds " _res #f))"))
                                      (else
                                       (string-append "     (if " rewrite-conds " _res #f)")))
                                "))))\n"))
                (else
                 (string-append " (" (att-value 'rule-clean-type _main-node) "\n"
                                "  (lambda (n)\n"
                                "   (rgd_match-pattern n (list "
                                (path->string _roots-paths)
                                (path->string _graphs-paths)
                                (if (null? _refs-paths) "" (path->string _refs-paths))
                                "))))"))))))))

