#lang racket/gui

(require racr/core)
(require rag-doll/base)
(require compatibility/mlist)

(define RagDollConnector%
  (class object%
    
    (init-field gui-menu)
    (init-field draw-area)
    (init-field area-w)
    (init-field area-h)
    (init-field user-spec)
    (field (ctrl-down #f))
    (field (copyNode null))
    (field (mainNode null)) 
    (field (last-nr 0))
    (field (typesView #f))
    (field (user-spec-data (mlist)))
    (field (rules-data null))
    (field (view-type 0))
    (field (types-list null))
    (field (node-r 0))
    (field (config null))
    
    
    (define/public (set-rewrite-conds _val)
      (rag-doll_set-rewrite-conds (string-replace _val "\\n" "\n")))
    
    
    (define/public (get-rewrite-conds)
      (string-replace (rag-doll_get-rewrite-conds) "\\n" "\n"))
    
    
    (define/public (create-mathing-commands)
      (let* ((_res (rag-doll_can-create-mathing-commands? config)) (_code (mcar _res)) (_msg (mcdr _res)))
        (cond ((= _code -1) (mcons 1 "No specification have been given!"))
              ((= _code -2) (mcons 1 "At least one node is required to create mathing commands!"))
              ((= _code -3) (mcons 1 "A distinguished node must be selected!"))
              ((= _code -4)
               (let ((_code (mcar _msg)))
                 (cond ((= _code 3)  (mcons 1 (string-append "The node number " (number->string (mcar (mcdr _msg))) " have invalid number of the children! This node should have " (number->string (mcdr (mcdr _msg))) " children node(s).")))
                       ((= _code 11) (mcons 1 (string-append "The reference from node " (number->string (mcar (mcdr _msg))) " to the node " (number->string (mcdr (mcdr _msg))) " has incorrect type!")))
                       (else         (mcons 1 (string-append "The node number " (number->string (mcdr _msg)) " have an invalid type!"))))))
              ((= _code -5) (mcons 1 (string-append "The node number " (number->string _msg) " is not reachable from the the distinguished node!")))
              ((= _code -6) (mcons 1 (string-append "The type of the distinguish node must be specified!")))
              ((= _code -7) (mcons 1 (string-append "The distinguish node cannot be a list node!")))
              ((= _code 0)
               (let ((_code (rag-doll_create-mathing-commands config)))
                 (if (string=? _code "")
                     (mcons 1 "Unexpected error occured! Could not create the rewrite function!")
                     (mcons 0 (mcons "The graph has the correct structure!" _code))))))))
    
    
    (define/public (set-default-data _data)
      (rag-doll_set-default-data _data))
    
    
    (define/public (get-rule-details _rule-name)
      (rag-doll_get-rule-details _rule-name))
    
    
    (define/public (get-rule-position _rule-name)
      (rag-doll_get-rule-position _rule-name))
    
    
    (define/public (get-rules-list)
      (mlist->list (rag-doll_get-rules-names)))
    
    
    (define/public (init _user-spec)
      (define-values (_w _h) (send (send draw-area get-dc) get-size))
      (set! config (rag-doll_init _user-spec _w _h))
      (let ((_types (rag-doll_get-rules-names)))
        (set! types-list (mlist (mlist "" (mlist 255 255 255)) (mlist "*" (mlist 200 200 200))))
        (mfor-each
         (lambda (_item)
           (set! types-list (mappend types-list (mlist (mlist _item (mlist (random 255) (random 255) (random 255)))))))
         _types)))
       
    
    (define/public (update-size)
      (define-values (_w _h) (send (send draw-area get-dc) get-size))
      (rag-doll_update-area-size config _w _h))
    
    
    (define/public (set-ctrl-down _val)
      (set! ctrl-down _val))
    
    
    (define/public (delete-type-from-list _type)
      (rag-doll_delete-type-from-list config _type))
    
    
    (define/public (replace-types-colors _new-types)
      (set! types-list _new-types))
    
    
    (define/public (get-types-list)
      types-list)
    
    
    (define/public (get-names-list)
      (rag-doll_get-names-list config))
    
    
    (define/public (repaint-area)
      (send (send draw-area get-dc) clear)
      (let ((_nodes (rag-doll_get-all-nodes config)))
        (mfor-each
         (lambda (_node)
           (print-node _node))
         _nodes)))
    
    
    (define/public (add-child _pos)
      (rag-doll_add-new-node config _pos))
    
    
    (define/public (auto-fill-children)
      (rag-doll_auto-fill-children config))
    
    
    (define/public (print-node _node)
      (let* ((_bitmap (make-node-bitmap _node)) (_x (rag-doll_get-node-x _node)) (_y (rag-doll_get-node-y _node)))
        (let ((_xPos (- _x (/ (send _bitmap get-width) 2))) (_yPos (- _y (/ (send _bitmap get-height) 2))) (_links (rag-doll_get-node-links _node node-r)))
          (mfor-each
           (lambda (_link)
            (let* ((_w (mlist-ref _link 2)) (_h (mlist-ref _link 3))
                   (_bitmap (make-bitmap _w _h)) (_bitmap-dc (new bitmap-dc% (bitmap _bitmap))))
              (if (equal? (mlist-ref _link 8) 'to-cut)
                  (send _bitmap-dc set-pen (make-color 200 200 200) 2 'solid)
                  (send _bitmap-dc set-pen (make-color 0 0 0) 2 'solid))
              (send _bitmap-dc draw-line (mlist-ref _link 4) (mlist-ref _link 5)
                                         (mlist-ref _link 6) (mlist-ref _link 7))
              (send (send draw-area get-dc) draw-bitmap _bitmap (mlist-ref _link 0) (mlist-ref _link 1))))
           _links)
          (send (send draw-area get-dc) draw-bitmap _bitmap _xPos _yPos)
          (let ((_refs (att-value 'references _node (rag-doll_get-curr-ref config))))
            (mfor-each
             (lambda (_ref)
               (let* ((_lines (mlist-ref _ref 1)) (_arrow (make-bitmap (rag-doll_get-area-w config) (rag-doll_get-area-h config))) (_arrow-dc (new bitmap-dc% (bitmap _arrow))))
                 (send _arrow-dc set-pen (make-color 200 0 0) (mlist-ref _ref 0) 'solid)
                 (mfor-each
                  (lambda (_section)
                    (send _arrow-dc draw-line (mlist-ref _section 0) (mlist-ref _section 1) (mlist-ref _section 2) (mlist-ref _section 3)))
                  _lines)
                 (send (send draw-area get-dc) draw-bitmap _arrow 0 0)))
             _refs)))))
  
    
    (define/public (save-state _fpath)
      (rag-doll_save-state config _fpath types-list))
    
    
    (define/public (load-state _fpath)
      (let ((_res (rag-doll_load-state config _fpath)))
        (cond ((string=? (mcar _res) "ok")
               (set! types-list (mcdr _res))
               (unset-curr-ref)
               (unset-curr-node)
               (repaint-area)
               "")
              (else
               (let ((_err (mcdr _res)))
                 (cond ((= _err -1) "File load error: Start symbol data is corrupted!")
                       ((= _err -2) "File load error: Main node data is corrupted!")
                       ((= _err -3) "File load error: User specification data is corrupted!")
                       (else        "File load error: File is corrupted!")))))))
    
    
    (define/private (make-node-bitmap _node)
      (let* ((_params (rag-doll_get-bitmap-data _node)) (_pen-s (mlist-ref _params 1)) (_size (mlist-ref _params 0)) (_bitmap (make-bitmap (+ _size _pen-s) (+ _size _pen-s))) (_dc (new bitmap-dc% [bitmap _bitmap])) (_margin (mlist-ref _params 2)) (_rgb '()))
        (set! _rgb (get-color-for-node _node))
        (send _dc set-pen "black" _pen-s 'solid)
        (send _dc set-brush (make-color (mlist-ref _rgb 0) (mlist-ref _rgb 1) (mlist-ref _rgb 2)) 'solid)
        (send _dc draw-ellipse _margin _margin _size _size)
        (send _dc set-font (make-font #:size (mlist-ref _params 3) #:weight 'bold))
        (send _dc draw-text (mlist-ref _params 4) (mlist-ref _params 5) (mlist-ref _params 6))
        _bitmap))
    
    
    (define/private (get-color-for-node _node)
      (cond ((in-types-view?)
             (let ((_type ""))
               (if (att-value 'is-list-node? _node)
                   (set! _type "*")
                   (set! _type (att-value 'rule-clean-type _node)))
               (let loop ((_list types-list))
                 (if (null? _list)
                     (mlist 0 0 0)
                     (begin
                       (let ((_item (mcar _list)))
                         (if (equal? (mlist-ref _item 0) _type)
                             (mlist-ref _item 1)
                             (loop (mcdr _list)))))))))
            ((in-references-view?)                 (mlist 255 255 255))
            ((rag-doll_is-node-main-to-cut? _node) (mlist 150 255 0))
            ((rag-doll_is-node-to-cut? _node)      (mlist 220 220 0))
            ((rag-doll_is-node-main? _node)        (mlist 0 200 0))
            (else                                  (mlist 170 170 0))))


    (define/public (clear-tree)
      (rag-doll_clear-tree config))
    
    
    (define/public (is-empty?)
      (rag-doll_is-tree-empty? config))
    
    
    (define/public (add-copied-child _list _pos _selFirst)
      (cond (_list
             (rag-doll_fill-tree config _list _pos _selFirst #f #f))))
    
    
    (define/public (set-types-view)
      (cond ((= view-type 1) (set! view-type 0))
            (else            (set! view-type 1))))

    
    (define/public (set-references-view)
      (cond ((= view-type 2) (set! view-type 0))
            (else            (set! view-type 2))))
    
    
    (define/public (set-names-view)
      (cond ((= view-type 3) (set! view-type 0))
            (else            (set! view-type 3))))
    
    
    (define/public (in-types-view?)      (= view-type 1))
    (define/public (in-references-view?) (= view-type 2))
    (define/public (in-names-view?)      (= view-type 3))
    
    
    (define/public (unset-curr-ref)
      (rag-doll_unset-curr-ref config)
      (send gui-menu activate-ref-gui-menu #f))
    
    
    (define/public (set-ref-as-curr _ref)
      (rag-doll_set-ref-as-curr config _ref)
      (send gui-menu activate-ref-gui-menu #t)
      (repaint-area))
    
    
    (define/public (get-curr-node)
      (rag-doll_get-curr-node config))
    
    
    (define/public (get-curr-ref)
      (rag-doll_get-curr-ref config))
    
    
    (define/public (delete-node _all)
      (cond ((is-curr-node-set?)
             (rag-doll_delete-curr-node config _all)
             (unset-curr-node)
             (send this repaint-area))))
    
    
    (define/public (delete-ref)
      (rag-doll_delete-curr-ref config))
    
    
    (define/public (move-node _side)
      (cond ((is-curr-node-set?)
             (rag-doll_move-node config _side)
             (send this repaint-area))))
    

    (define/public (is-in-circle _nray _mouseX _mouseY _circX _circY)
      (let ([dist (expt (+ (expt (- _mouseX _circX) 2) (expt (- _mouseY _circY) 2)) 0.5)])
        (< dist _nray)))
    
    
    (define/private (add-reference _refto)
      (rag-doll_add-reference config _refto))
    
    
    (define/public (get-root-node)
      (rag-doll_get-root-node config))
    
    
    (define/public (find-clicked-node _mouseX _mouseY)
      (let* ((curr-node (rag-doll_get-curr-node config)) (_leftB (inexact->exact (floor (/ _mouseX (rag-doll_get-node-x-space curr-node))))) (_border (cons _leftB (+ _leftB 1))) (_nray (rag-doll_get-node-ray curr-node)))
        (call/cc
         (lambda (break)
           (let loop ((_node (rag-doll_get-root-node config)))
             (let ((_children (rag-doll_get-node-children _node)))
               (ast-for-each-child
                (lambda (i _child)
                  (let ((_restr (rag-doll_get-node-range-restriction _child)))
                    (cond ((and (>= (car _border) (mcar _restr)) (<= (cdr _border) (mcdr _restr)))
                           (if (is-in-circle _nray _mouseX _mouseY (rag-doll_get-node-x _child) (rag-doll_get-node-y _child))
                               (begin
                                 (cond ((not (= (ast-child 'nr _child) (ast-child 'nr curr-node)))
                                        (cond ((and (in-references-view?) ctrl-down)
                                               (add-reference _child))
                                              (else
                                               (unset-curr-ref)
                                               (set-curr-node _child)))
                                        (send this repaint-area)))
                                 (break 0))
                               (begin
                                 (loop _child)))))))
                _children)))
           (cond ((not (and (in-references-view?) ctrl-down))
                  (cond ((unset-curr-node)
                         (send this repaint-area)))))
           (break -1)))))
          
    
    (define/public (find-clicked-references _mx _my)
      (let ((_reflist (mlist)))
        (let loop ((_n (rag-doll_get-root-node config)))
          (let ((_i 1))
            (mfor-each
             (lambda (_ref)
               (mfor-each
                (lambda (_line)
                  (let ((_sx (mlist-ref _line 0)) (_sy (mlist-ref _line 1)) (_ex (mlist-ref _line 2)) (_ey (mlist-ref _line 3)) (_pom 0))
                    (cond ((<= (abs (- (* (- _ex _sx) (- _my _sy)) (* (- _ey _sy) (- _mx _sx)))) 900)
                           (cond ((> _sx _ex) (set! _pom _sx) (set! _sx _ex) (set! _ex _pom)))
                           (cond ((> _sy _ey) (set! _pom _sy) (set! _sy _ey) (set! _ey _pom)))
                           (cond ((or (and (= _sy _ey) (>= _mx _sx) (<= _mx _ex) (<= (abs (- _ey _my)) 5))
                                      (and (= _sx _ex) (>= _my _sy) (<= _my _ey) (<= (abs (- _ex _mx)) 5))
                                      (and (>= _mx _sx) (<= _mx _ex) (>= _my _sy) (<= _my _ey)))
                                  (set! _reflist (mappend _reflist (mlist (rag-doll_get-node-reference-on-pos _n _i))))))))))
                (mlist-ref _ref 1))
               (set! _i (+ _i 1)))
             (rag-doll_get-node-converted-references _n))
          (let ((_children (rag-doll_get-node-children-as-list _n)))
            (mfor-each
             (lambda (_child)
               (loop _child))
             _children))))
        (unset-curr-ref)
        (cond ((not (null? _reflist))
               (set-ref-as-curr (mlist-ref _reflist 0))))
        _reflist))
    
    
    (define/public (is-copy-node-set?)
      (rag-doll_is-copy-node-set? config))
    
    
    (define/public (is-cut-node-set?)
      (rag-doll_is-cut-node-set? config))
    
    
    (define/public (paste-cutted-node _pos)
      (rag-doll_paste-cutted-node config _pos))
    
    
    (define/public (set-curr-as-main)
      (cond ((is-curr-node-set?)
             (rag-doll_set-curr-as-main config)
             (repaint-area))))
    
    
    (define/public (get-node-index-by-nr _number)
        (ast-child-index (rag-doll_find-node config _number)))


    (define/public (set-curr-node _node)
      (rag-doll_set-curr-node config _node)
      (send gui-menu activate-node-gui-menu #t))
    
    
    (define/public (unset-curr-node)
      (if (not (is-curr-node-set?))
          #f
          (begin
            (rag-doll_unset-curr-node config)
            (send gui-menu activate-node-gui-menu #f))))
    
    
    (define/public (set-curr-node-to-cut)
      (cond ((is-curr-node-set?)
             (rag-doll_set-curr-node-to-cut config)
             (repaint-area))))
    
    
    (define/public (is-any-node-set-to-cut?)
      (rag-doll_is-any-node-set-to-cut? config))
    
    
    (define/public (set-curr-node-to-copy)
      (cond ((rag-doll_set-curr-node-to-copy config)
             (rag-doll_serialize-copy-node config))
            (else
             #f)))
    
    
    (define/public (save-type-if-new _type)
      (call/cc
       (lambda (break)
         (mfor-each
          (lambda (_item)
            (cond ((equal? (mlist-ref _item 0) _type)
                   (break #t))))
          types-list)
         (let ((_color (mlist (random 255) (random 255) (random 255))))
           (set! types-list (rag-doll_call-qsort-string (mappend types-list (mlist (mlist _type _color))))))
         (break #f))))


    (define/public (set-new-type _type)
      (let* ((_res (rag-doll_save-new-type config _type)))
        (cond ((= _res 0)  (cons "Node's data have been successfuly updated." 0))
              ((= _res 1)  (cons "Reference's data have been successfuly updated." 0))
              ((= _res -2) (cons (string-append "The type '" _type "' does not exist in the specification!") 1)))))
    
    
    (define/public (set-new-name _type)
      (rag-doll_save-new-name config _type))
    
    
    (define/public (get-add-node-positions-list)
      (define (loop _list)
        (append
         (list (string-append "after " (number->string (mcar _list))))
         (if (null? (mcdr _list))
             (list)
             (loop (mcdr _list)))))
      (let ((_nums (rag-doll_get-curr-node-children-numbers config)))
        (append
         (list "as first")
         (if (null? _nums)
             (list)
             (loop _nums))
         (list "as last"))))
    
    
    (define/public (get-curr-node-name)
      (rag-doll_get-curr-node-name config))
    
    
    (define/public (get-curr-node-type)
      (rag-doll_get-curr-node-type config))
    
    
    (define/public (get-curr-node-type-for-ast-info)
      (rag-doll_get-curr-node-type-for-ast-info config))
    
    
    (define/public (is-curr-node-list-node?)
      (rag-doll_is-curr-node-list-node? config))
    
    
    (define/public (get-name-to-print)
      (rag-doll_get-name-of-sel-obj config))
    
      
    (define/public (get-type-to-print)
      (rag-doll_get-text-of-sel-obj config))
    
      
    (define/public (get-curr-node-nr)
      (rag-doll_get-curr-node-nr config))
    
    
    (define/public (is-curr-node-set?)
      (rag-doll_is-curr-node-set? config))
    
    
    (define/public (is-curr-ref-set?)
      (rag-doll_is-curr-ref-set? config))
    
    
    (define/public (set-curr-node-by-nr _nr)
      (rag-doll_set-curr-node-by-nr config _nr)
      (send gui-menu activate-node-gui-menu #t))
    
    
    (define/public (update-node-menu)
      (update-children-cb #t)
      (let ((_new-text (if (in-names-view?) (get-name-to-print) (get-type-to-print))))
        (send gui-menu set-tb-nodeText-value _new-text)
        (send gui-menu refresh-tb-nodeText)
        _new-text))
    
    
    (define/public (update-children-cb _set-default)
      (let ((_list (get-add-node-positions-list)))
        (send gui-menu update-combo-addChild-choices _list)
        (cond ((eq? _set-default #t)
               (send gui-menu set-combo-addChild-value "as last")))))
    
    
    (super-new)))

(provide RagDollConnector%)
