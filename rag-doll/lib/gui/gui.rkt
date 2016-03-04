#lang racket/gui

(require racr/core)
(require rag-doll/util)
(require rag-doll/base)
(require rag-doll/lib/gui/rag-doll-connector)
(require rag-doll/lib/gui/lists/references-list)
(require rag-doll/lib/gui/lists/types-list)
(require rag-doll/lib/gui/lists/names-list)
(require rag-doll/lib/gui/gui-menu)
(require rag-doll/lib/gui/windows)
(require compatibility/mlist)

(define RagDollWin%
  (class object%
    
    (init-field win-width)
    (init-field win-height)
    (init-field default-data)
    (init-field src-path)
    (init-field win-manager)
    (field (frame null))
    (field (main-panel null))
    (field (panels null))
    (field (l-area null))
    (field (l-tree-cont null))
    (field (references-list null))
    (field (references-list-panel null))
    (field (references-list-cont null))
    (field (types-list null))
    (field (types-list-panel null))
    (field (types-list-cont null))
    (field (names-list null))
    (field (names-list-panel null))
    (field (names-list-cont null))
    (field (rgb-hpanel null))
    (field (rgb-panel null))
    (field (copy-area null))
    (field (copy-area-cont null))
    (field (tests-panel null))
    (field (bottom-panel null))
    (field (status-box null))
    (field (conditions-box null))
    (field (tb-R null))
    (field (tb-G null))
    (field (tb-B null))
    (field (rgb-preview null))
    (field (opts1-panel null))
    (field (opts2-panel null))
    (field (msg-tab null))
    (field (conditions-tab null))
    (field (ast-rules-tab null))
    (field (clipboard-tab null))
    (field (rgb-tab null))
    (field (ctrl-down #f))
    (field (gui-menu null))
    (field (menu-save null))
    (field (menu-node null))
    (field (menu-ref null))
    (field (ast-rules-list null))
    (field (ast-rule-fields null))
    (field (ast-rule-fields-list null))
    (field (curr-nodeText-value ""))
    (field (last-file ""))
    (field (win-parent null))
    (field (id 0))
    (field (is-shown-val? #t))
    (field (min-win-width 1000))
    (field (min-win-height 250))
    (field (menu-height 50))
    (field (bottom-menu-height 145))
    (field (bottom-tabs-width 0))
    (field (file-select-dialog '()))
    (field (mathing-commands-dialog '()))
    
    
    ; ------------------- CLASSES -------------------
    (define my-frame%
      (class frame% 
        (define/override (on-subwindow-char el event) (graph-area_key-event event) #f)
        (define/override (on-superwindow-show _val) (set! is-shown-val? _val))
        (define/override (on-subwindow-event el event)
          (cond ((and (equal? el ast-rules-list) (equal? 'left-up (send event get-event-type)))
                 (let ((_sels (send ast-rules-list get-selections)))
                   (cond ((not (null? _sels))
                          (update-ast-rules-info (list-ref (send l-tree-cont get-rules-list) (car _sels))))))))
          #f)
        (super-new)))
    
    
    (define list-canvas%
      (class canvas%
        (field (list-cont null))
        (define/public (set-list-cont lc) (set! list-cont lc))
        (define/override (on-event event)
          (cond ((send event moving?)       (send list-cont check-hover (send event get-y)))
                ((send event leaving?)      (send list-cont clear-mouseover))
                ((send event get-left-down) (send list-cont clicked (send event get-y)))))
        (define/override (on-char event)    (cond ((eq? (send event get-key-release-code) #\rubout) (send list-cont delete-curr))))
        (super-new)))
    
    
    (define list-canvas-types%
      (class list-canvas% 
        (inherit-field list-cont)
        (inherit set-list-cont)
        (define/override (on-event event)
          (cond ((send event moving?)  (send list-cont check-hover (send event get-y)))
                ((send event leaving?) (send list-cont clear-mouseover))
                ((send event get-left-down)
                 (let ((_res (send list-cont clicked (send event get-y))))
                   (cond ((and (not (null? _res)) (not (number? _res)))
                          (activate-rgb-tab 'typeslist (mlist-ref _res 1))
                          (update-ast-rules-info (mlist-ref _res 0)))
                         (else
                          (deactivate-rgb-tab)))))))
        (define/override (on-char event) (cond ((eq? (send event get-key-release-code) #\rubout) (send list-cont delete-curr))))
        (super-new)))
    
    
    (define my-combo-field%
      (class combo-field%
        (super-new)
        (inherit get-menu append)
        (define/public (update-choices choice-list)
          (map (lambda (i) (send i delete)) (send (get-menu)        get-items))
          (map (lambda (choice-label)       (append choice-label))  choice-list)
          (void))
        (define/public (add-choice _choice)
          (let ((_items (send (get-menu) get-items))) (append (number->string _choice)))       
          (void))))
    
    
    (define my-canvas%
      (class canvas% 
        (define/override (on-event event) (graph-area_mouse-event event))
        (super-new)))
    
    
    (define (open-create-file-select-dialog _var1 _var2 _var3 _var4 _var5)
      (cond ((or (null? file-select-dialog) (not (send file-select-dialog is-shown?)))
             (set! file-select-dialog (create-file-select-dialog frame _var1 _var2 _var3 _var4 _var5)))))
    
    
    ; --------------- INIT FUNCTION ---------------
    (define/public (gui-init _win-parent _user-spec _id)
      (define (set-main-menu _frame)
        (define mb (new menu-bar% [parent frame]))
        (set! win-parent _win-parent)
        (set! id _id)
        (cond ((< win-width min-win-width) (set! win-width min-win-width)))
        (cond ((< win-height min-win-height) (set! win-height min-win-height)))
        (let ([m (new menu% [parent mb] [label "&File"])])
          (new menu-item% [parent m] [label "&New"] [callback (lambda (b e) (send win-parent new-project))])
          (new separator-menu-item% [parent m])
          (new menu-item% [parent m] [label "&Open"] [callback (lambda (b e) (open-create-file-select-dialog "Open" "Open" #t this 3))])
          (new menu-item% [parent m] [label "&Open in new window"] [callback (lambda (b e) (open-create-file-select-dialog "Open" "Open" #t this 1))])
          (set! menu-save (new menu-item% [parent m] [label "&Save"] [callback (lambda (b e) (save-state ""))]))
          (new menu-item% [parent m] [label "&Save as"] [callback (lambda (b e) (open-create-file-select-dialog "Save as" "Save"  #f this 2))])
          (new separator-menu-item% [parent m])
          (new menu-item% [parent m] [label "&Exit"] [callback (lambda (b e) (exit-project))])
          (send menu-save enable #f)
          (send gui-menu enable-save-butt #f))
        (let ([m (new menu% [parent mb] [label "&Views"])])
          (new menu-item% [parent m] [label "&References"] [callback (lambda (b e) (references-view))])
          (new menu-item% [parent m] [label "&Nodes types"] [callback (lambda (b e) (types-view))])
          (new menu-item% [parent m] [label "&Nodes names"] [callback (lambda (b e) (names-view))]))
        (let ([m (new menu% [parent mb] [label "&Info"])])
          (new menu-item% [parent m] [label "&Help"] [callback (lambda (b e) (message-box "Help" "For documentation please consult the RACR hompage at \"https://code.google.com/p/racr/\". For any remaining questions or comments don't hesitate to write an e-mail to \"Christoff.Buerger@gmail.com\". Any feedback is appreciated." frame))])
          (new menu-item% [parent m] [label "&About"] [callback (lambda (b e) (message-box "About" "        Rag Doll version 1.0.1        \n\n        Author: Adam Misiuda        "))])))
        
      ; prepare source path
      (if (or (null? _user-spec) (not (= (specification->phase _user-spec) 3)))
          (show-message "The specyfication must be compailed!" 1)
          (begin
            (set! frame (new my-frame% (label "Rag Doll") (width win-width) (height win-height) (stretchable-width #f) (stretchable-height #f)))
            (set! main-panel (new vertical-panel% (parent frame)))
            ; ------------------ MENU ------------------
            (let ((menu-panel (new horizontal-panel% (parent main-panel) (stretchable-height #f) (spacing 5) (min-height menu-height))))
              (set! gui-menu (new GuiMenu%))
              (let ((_panel (new horizontal-panel% (parent menu-panel))))
                (send gui-menu set-save-state-butt (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/save.png"))) [stretchable-width #f] (callback (lambda (button event) (save-state "")))))
                (send gui-menu set-load-state-butt (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/load.png"))) [stretchable-width #f] (callback (lambda (button event) (open-create-file-select-dialog "Open" "Open" #t this 3)))))
                (send gui-menu set-ctrl-down-butt  (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/ctrl.png"))) (callback (lambda (button event) (toggle-control-down)))))
                (send gui-menu set-fun-gen-butt    (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/test.png"))) [stretchable-width #f] (callback (lambda (button event) (check-with-spec))))))
              (let ((_panel (new horizontal-panel% (parent menu-panel))))
                (send gui-menu set-names-view-butt (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/names.png"))) (callback (lambda (button event) (names-view)))))
                (send gui-menu set-types-view-butt (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/types.png"))) (callback (lambda (button event) (types-view)))))
                (send gui-menu set-refs-view-butt  (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/refs.png"))) (callback (lambda (button event) (references-view))))))
              (let ((_panel (new horizontal-panel% (parent menu-panel))))
                (send gui-menu set-combo-addChild  (new my-combo-field% (parent _panel) (label "Insert: ") (min-width 120) (init-value "as last") (choices (list))))
                (send gui-menu set-add-butt        (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/add.png"))) (callback (lambda (textField event) (add-new-child)))))
                (send gui-menu set-delete-butt     (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/delete.png"))) (enabled #f) (callback (lambda (textField event) (delete-node #f)))))
                (send gui-menu set-delete-all-butt (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/delete-all.png"))) (enabled #f) (callback (lambda (textField event) (delete-node #t)))))
                (send gui-menu set-left-butt       (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/left.png"))) (enabled #f) (callback (lambda (button event) (move-node -1)))))
                (send gui-menu set-right-butt      (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/right.png"))) (enabled #f) (callback (lambda (button event) (move-node 1)))))
                (send gui-menu set-home-butt       (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/home.png"))) (enabled #f) (callback (lambda (button event) (set-main-node)))))
                (send gui-menu set-copy-butt       (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/copy.png"))) (enabled #f) (callback (lambda (button event) (copy-cut 1)))))
                (send gui-menu set-cut-butt        (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/cut.png"))) (enabled #f) (callback (lambda (button event) (copy-cut 2)))))
                (send gui-menu set-paste-butt      (new button% (parent _panel) (label (read-bitmap (string-append src-path "i/paste.png"))) (enabled #f) (callback (lambda (button event) (paste-node)))))
                (send gui-menu set-tb-nodeText     (new text-field% (parent _panel) (label "Type: ") (enabled #f) (callback (lambda (textField event) (type-tf-key-enter event)))))))
            (set-main-menu frame)
            
            ; --- MIDDLE PANELS ---
            (set! panels (new horizontal-panel% (parent main-panel) (spacing 5)))
            (set! l-area
                  (new my-canvas% [parent panels] [min-width (- win-width 200)] [min-height (- (- win-height menu-height) bottom-menu-height)]
                       (paint-callback (lambda (canvas dc) (send l-tree-cont update-size) (send l-tree-cont repaint-area)))))
            (set! l-tree-cont (new RagDollConnector% (gui-menu gui-menu) (draw-area l-area) (area-w 400) (area-h 300) (user-spec _user-spec)))
            (send l-tree-cont init _user-spec)
            
            (set! types-list-panel (new vertical-panel% (parent panels) (min-width 0) [stretchable-width #f]))
            (set! types-list (new list-canvas-types% [parent types-list-panel] (min-width 0) (stretchable-width #f)
                                  (paint-callback (lambda (canvas dc)
                                                    (send types-list-cont update-size)
                                                    (cond ((send l-tree-cont in-types-view?)
                                                           (send types-list-cont repaint)))))))
            (set! types-list-cont (new MyTypesList% (draw-area types-list) (win-parent this) (tree-class l-tree-cont) (area-w 200) (area-h 0)))
            (send types-list set-list-cont types-list-cont)
            
            (set! references-list-panel (new vertical-panel% (parent panels) (min-width 0) [stretchable-width #f]))
            (set! references-list (new list-canvas% [parent references-list-panel] (min-width 0) (stretchable-width #f)
                                       (paint-callback (lambda (canvas dc) (send references-list-cont update-size) (send references-list-cont repaint)))))
            (set! references-list-cont (new MyReferencesList% (draw-area references-list) (win-parent this) (tree-class l-tree-cont) (area-w 200) (area-h 0)))
            (send references-list set-list-cont references-list-cont)
            
            (set! names-list-panel (new vertical-panel% (parent panels) (min-width 0) [stretchable-width #f]))
            (set! names-list (new list-canvas% [parent names-list-panel] (min-width 0) (stretchable-width #f)
                                  (paint-callback (lambda (canvas dc) (send names-list-cont update-size) (send names-list-cont repaint)))))
            (set! names-list-cont (new MyNamesList% (draw-area names-list) (win-parent this) (tree-class l-tree-cont) (area-w 200) (area-h 0)))
            (send names-list set-list-cont names-list-cont)
            
            ; --- BOTTOM PANEL --- 
            (set! bottom-tabs-width (- win-width 190))
            (set! bottom-panel (new horizontal-panel% (parent main-panel) (spacing 0) (stretchable-height #f) (min-height bottom-menu-height)))
            
            (set! opts1-panel
                  (instantiate tab-panel% ('("Messages" "AST Rules" "Rewrite conditions") bottom-panel)
                    (callback
                     (lambda (tp e)
                       (case (send tp get-selection)
                         ((0) (send tp change-children (lambda (children) (list msg-tab))))
                         ((1) (send tp change-children (lambda (children) (list ast-rules-tab))))
                         ((2) (send tp change-children (lambda (children) (list conditions-tab)))))))))
            (set! msg-tab (instantiate panel% (opts1-panel)))
            (set! status-box (new text-field% (parent msg-tab) (label "") (style '(multiple)) (min-width bottom-tabs-width) (min-height 110)))
            (set! ast-rules-tab (instantiate panel% (opts1-panel) (stretchable-height #f)))
            (let ((_panel (new horizontal-panel% (parent ast-rules-tab))))
              (set! ast-rules-list (new list-box% (parent _panel) (label "") (columns '("Rules list")) (style '(single column-headers)) (choices '()) (min-width 200) (min-height 100)))
              (let ((_vpanel (new vertical-panel% (parent _panel))))
                (set! ast-rule-fields-list (new list-box% (parent _vpanel) (label "") (style '(single column-headers)) (columns '("Inheritance list")) (choices '("")) (min-width (- bottom-tabs-width 230)) (min-height 78)))
                (set! ast-rule-fields (new text-field% (parent _vpanel) (label "Rule:") (enabled #f) (min-width (- bottom-tabs-width 205))))))
            (set! conditions-tab (instantiate panel% (opts1-panel)))
            (let ((_panel (new vertical-panel% (parent conditions-tab))))
              (set! conditions-box (new text-field% (parent _panel) (label "") (style '(multiple)) (min-width bottom-tabs-width) (min-height 75)))
              (new button% (parent _panel) (label "Save conditions") (callback (lambda (button event) (save-rewrite-conds)))))
            (send opts1-panel change-children (lambda (children) (list msg-tab)))
            
            (set! opts2-panel
                  (instantiate tab-panel%
                    ('("Clipboard" "Color pallete") bottom-panel) (stretchable-width #f) (stretchable-height #f)
                    (callback
                     (lambda (tp e)
                       (case (send tp get-selection)
                         ((0) (send tp change-children (lambda (children) (list clipboard-tab))))
                         ((1) (send tp change-children (lambda (children) (list rgb-tab)))))))))
            (set! clipboard-tab (instantiate panel% (opts2-panel) (min-height 114) (min-width 210)))
            (set! rgb-tab (instantiate panel% (opts2-panel) (enabled #f) (min-height 114) (min-width 210)))
            (set! copy-area
                  (new my-canvas% [parent clipboard-tab] (min-width 200) (min-height 110) [stretchable-width #f] [stretchable-height #f]
                       (paint-callback (lambda (canvas dc) (send copy-area-cont repaint-area)))))
            (set! copy-area-cont (new RagDollConnector% (gui-menu gui-menu) (draw-area copy-area) (area-w 200) (area-h 110) (user-spec _user-spec)))
            (send copy-area-cont init _user-spec)
            (set! rgb-hpanel (new horizontal-panel% (parent rgb-tab) (vert-margin 5) (spacing 5) (min-width 0)  (min-height 30) (stretchable-width #f) [stretchable-height #f]))
            (set! rgb-panel (new vertical-panel% (parent rgb-hpanel) (spacing 10) (min-width 0)  (min-height 30) (stretchable-width #f) [stretchable-height #f]))
            (set! tb-R (new text-field% (parent rgb-panel) (label "R") (min-width 50) (stretchable-width #f) (callback (lambda (textField event) (type-rgb-tf-key-enter 0 (send event get-event-type))))))
            (set! tb-G (new text-field% (parent rgb-panel) (label "G") (min-width 50) (stretchable-width #f) (callback (lambda (textField event) (type-rgb-tf-key-enter 1 (send event get-event-type))))))
            (set! tb-B (new text-field% (parent rgb-panel) (label "B") (min-width 50) (stretchable-width #f) (callback (lambda (textField event) (type-rgb-tf-key-enter 2 (send event get-event-type))))))
            (set! rgb-preview (new canvas% (parent rgb-hpanel) (min-width 77) (min-height 90) (stretchable-width #f) [stretchable-height #f]))
            (show-clipboard)
            
            (send frame show #t)
            (send l-tree-cont update-size)
            (send copy-area-cont update-size)
            (send types-list-cont update-size)
            (send references-list-cont update-size)
            (send l-tree-cont set-default-data default-data)
            (update-ast-rules-list))))


    (define/public (is-shown?)
      is-shown-val?)
    
    
    (define/private (update-ast-rules-info _rule)
    (let ((_list '()) (_field-text "") (_rule-pos -1))
      (cond ((not (null? _rule))
             (cond ((string=? _rule "")
                    (cond ((send l-tree-cont is-curr-node-set?) (set! _field-text "<< No type has been specified for this node >>"))))
                   ((string=? _rule "*")
                    (set! _field-text "<< This is a list node >>"))
                   ((and (not (null? _rule)) (string? _rule))
                    (let ((_len (- (string-length _rule) 1)))
                      (cond ((string=? "*" (substring _rule _len))
                             (set! _rule (substring _rule 0 _len)))))
                    (let ((_rdets (send l-tree-cont get-rule-details _rule)) (_list2 '()) (_str ""))
                      (cond ((> (mlength _rdets) 1)
                             (mfor-each
                              (lambda (_item)
                                (set! _list (append _list (list (symbol->string (mlist-ref _item 1))))))
                              (mcdr _rdets))))
                      (cond ((not (null? _rdets))
                             (set! _field-text (symbol->string (mlist-ref (mcar _rdets) 1))))))
                    (let ((_ret (send ast-rules-list find-string _rule)))
                      (cond (_ret (set! _rule-pos _ret))))))))
      (send ast-rule-fields-list set _list)
      (send ast-rule-fields set-value _field-text)
      (if (> _rule-pos -1)
          (send ast-rules-list select _rule-pos)
          (let ((_pos (send ast-rules-list get-selections)))
            (cond ((not (null? _pos)) (send ast-rules-list select (car _pos) #f)))))
      (cond ((not (= (send opts1-panel get-selection) 1))
             (send opts1-panel change-children (lambda (children) (list ast-rules-tab)))
             (send opts1-panel set-selection 1)))))

    
    (define (save-rewrite-conds)
      (if (send l-tree-cont set-rewrite-conds (send conditions-box get-value))
          (show-message "Rewrite conditions have been correctly saved." 0)
          (show-message "Unexpected error occured while saving the rewrite conditions!." 1)))
    
    
    (define/private (update-ast-rules-list)
      (let ((_rules (send l-tree-cont get-rules-list)))
        (send ast-rules-list set _rules)))
    
    
    (define/public (save-state _fpath)
      (if (string=? _fpath "")
          (set! _fpath last-file)
          (set! last-file _fpath))
      (cond ((string=? "" _fpath)
             (message-box "No file!" "No file have been selected!"))
            (else
             (send menu-save enable #t)
             (send gui-menu enable-save-butt #t)
             (send l-tree-cont save-state _fpath)
             (send frame set-label (string-append "Rag Doll [" _fpath "]"))
             (show-message (string-append "Data have been saved correctly into the file '" _fpath "'.") 0))))
    
    
    (define/public (get-opened-file)
      last-file)
    
    
    (define/public (load-state _fpath)
      (let ((_ret #f))
        (cond ((not (list-contains (send win-manager get-opened-files) _fpath))
               (let ((_res (send l-tree-cont load-state _fpath)))
                 (cond ((string=? _res "")
                        (set! last-file _fpath)
                        (send menu-save enable #t)
                        (send gui-menu enable-save-butt #t)
                        (cond ((send l-tree-cont in-types-view?)      (types-view))
                              ((send l-tree-cont in-references-view?) (references-view))
                              ((send l-tree-cont in-names-view?)      (names-view)))
                        (send gui-menu activate-base-gui-menu #t)
                        (update-children-cb #t)
                        (send frame set-label (string-append "Rag Doll [" _fpath "]"))
                        (clear-clipboard)
                        (send conditions-box set-value (send l-tree-cont get-rewrite-conds))
                        (show-message "Data have been loaded correctly." 0)
                        (set! _ret #t))
                       (else
                        (show-message _res 1)))))
              (else
               (show-message (string-append "File '" _fpath "' is already opened") 1)))
        (update-ast-rules-list)
        _ret))
    
    
    (define/private (clear-clipboard)
      (send copy-area-cont clear-tree))
    
    
    (define/public (open-project _fpath)
      (send win-parent open _fpath))
    
    
    (define/public (load-project _fpath)
      (cond ((send win-parent can-load? _fpath)
             (load-state _fpath)
             (send win-parent load id _fpath))))


    (define/public (save-project _fpath)
      (cond ((send win-parent save id _fpath)
             (save-state _fpath))))

    (define/public (exit-project)
      (send win-parent close id)
      (send frame on-exit))

  
    (define/public (focus)
      (send frame focus))
    

    (define/private (show-message _txt _type)
      (send status-box set-value _txt)
      (let ((_color (make-object color% 255 255 255)))
        (cond ((= _type 0) (set! _color (make-object color% 220 255 220)))
              ((= _type 1) (set! _color (make-object color% 255 220 220))))
        (send status-box set-field-background _color)
        (cond ((not (= (send opts1-panel get-selection) 0))
               (send opts1-panel change-children (lambda (children) (list msg-tab)))
               (send opts1-panel set-selection 0)))))
    
    
    (define/private (clear-message-box)
      (send status-box set-value "")
      (send status-box set-field-background (make-object color% 255 255 255)))
    
    
    (define/private (graph-area_mouse-event event)
      (cond ((and (not (send event moving?)) (send event get-left-down))
             (let ((_refs null) (_repaint #f) (_node-selected? (send l-tree-cont is-curr-node-set?)) (_ref-selected? (send l-tree-cont is-curr-ref-set?)))
               (let ((_clicked-ret (send l-tree-cont find-clicked-node (send event get-x) (send event get-y))))
                 (cond ((= _clicked-ret 0)
                        (cond ((send l-tree-cont in-types-view?)
                               (let ((_ret (send types-list-cont select-type (send l-tree-cont get-curr-node-type))))
                                 (cond ((not (null? _ret))
                                        (activate-rgb-tab 'typeslist (mlist-ref _ret 1))))))
                              ((send l-tree-cont in-names-view?)
                               (send names-list-cont select-node-nr (send l-tree-cont get-curr-node-nr))))
                        (update-node-menu))
                       (else
                        (set! _refs (send l-tree-cont find-clicked-references (send event get-x) (send event get-y)))
                        (cond ((not (null? _refs))
                               (set! _repaint #t))
                              (_ref-selected?
                               (send l-tree-cont unset-curr-ref)
                               (set! _repaint #t))
                              (else
                               (send gui-menu activate-base-gui-menu #t)
                               (update-children-cb #t)
                               (cond ((send l-tree-cont in-types-view?) (send types-list-cont clear-selection)  (send types-list-cont repaint))
                                     ((send l-tree-cont in-names-view?) (send names-list-cont clear-selection)  (send names-list-cont repaint)))))
                        (update-ast-rules-info null)
                        (cond (_repaint
                               (send l-tree-cont repaint-area)
                               (update-node-menu))))))
               (cond ((send l-tree-cont in-references-view?)
                      (send references-list-cont update-list _refs)))))))
    
    
    
    (define/private (graph-area_key-event event)
      (let ((_key (send event get-key-code)))
        (cond ((equal? _key 'control) (set-control-down #t))
              ((equal? _key 'release) (set-control-down #f)))))
    
    
    (define/private (set-control-down _b)
      (cond ((not (eq? _b ctrl-down))
             (toggle-control-down))))
    
    
    (define/private (toggle-control-down)
      (set! ctrl-down (not ctrl-down))
      (if ctrl-down
          (send gui-menu set-ctrl-down-butt-label (read-bitmap "src/i/ctrl-active.png"))
          (send gui-menu set-ctrl-down-butt-label (read-bitmap "src/i/ctrl.png")))
      (send l-tree-cont set-ctrl-down ctrl-down))
    
    
    (define/private (types-view)
      (cond ((send l-tree-cont in-references-view?) (close-references-view)))
      (cond ((send l-tree-cont in-names-view?)      (close-names-view)))
      (if (send l-tree-cont in-types-view?)
          (close-types-view)
          (begin
            (send types-list min-client-width 200)
            (show-rbg-tab)
            (send gui-menu set-types-view-butt-label (read-bitmap "src/i/types-active.png"))
            (send types-list-cont select-type (send l-tree-cont get-curr-node-type))))
      (send l-tree-cont set-types-view)
      (update-types-view))
    
    
    (define/private (references-view)
      (cond ((send l-tree-cont in-types-view?) (close-types-view))
            ((send l-tree-cont in-names-view?) (close-names-view)))
      (cond ((send l-tree-cont in-references-view?)
             (close-references-view))
            (else
             (send references-list min-client-width 200)
             (send references-list-cont update-list null)
             (send references-list-cont repaint)
             (send gui-menu set-refs-view-butt-label (read-bitmap "src/i/refs-active.png"))))
      (send l-tree-cont set-references-view))
    
    
    (define/private (names-view)
      (cond ((send l-tree-cont in-references-view?) (close-references-view))
            ((send l-tree-cont in-types-view?)      (close-types-view)))
      (cond ((send l-tree-cont in-names-view?)
             (close-names-view))
            (else
             (send names-list min-client-width 200)
             (send names-list-cont update-list)
             (send names-list-cont repaint)
             (send gui-menu set-tb-nodeText-label "Name: ")
             (send gui-menu set-names-view-butt-label (read-bitmap "src/i/names-active.png"))
             (set-new-node-text-value (send l-tree-cont get-name-to-print))
             (send l-tree-cont set-names-view)
             (send names-list-cont select-node-nr (send l-tree-cont get-curr-node-nr)))))
    
    
    (define/private (update-views)
      (cond ((not (update-names-view))
             (update-types-view))))
    
    
    (define/private (set-new-node-text-value _text)
      (set! curr-nodeText-value _text)
      (send gui-menu set-tb-nodeText-value _text))
    
    
    (define/private (close-names-view)
      (send gui-menu set-names-view-butt-label (read-bitmap "src/i/names.png"))
      (send names-list min-client-width 0)
      (send names-list-cont clear-selection)
      (send gui-menu set-tb-nodeText-label "Type: ")
      (send l-tree-cont set-names-view)
      (update-node-menu))
    
    
    (define/private (close-types-view)
      (send gui-menu set-types-view-butt-label (read-bitmap "src/i/types.png"))
      (send types-list min-client-width 0)
      (show-clipboard)
      (send types-list-cont clear-selection)
      (deactivate-rgb-tab))
    
    
    (define/private (close-references-view)
      (send gui-menu set-refs-view-butt-label (read-bitmap "src/i/refs.png"))
      (send references-list min-client-width 0)
      (send references-list-cont clear-selection))
    
    
    (define/private (update-types-view)
      (cond ((send l-tree-cont in-types-view?)
             (send types-list-cont update-list))))
    
    
    (define/private (update-names-view)
      (cond ((send l-tree-cont in-names-view?)
             (send names-list-cont update-list)
             (send names-list-cont repaint)
             #t)
            (else
             #f)))
    
    
    (define/private (set-main-node)
      (if (send l-tree-cont is-curr-node-list-node?)
          (show-message "A list node cannot be a distinguished node!" 1)
          (send l-tree-cont set-curr-as-main)))
    
    
    (define/private (add-new-child)
      (cond ((and ctrl-down (> (send l-tree-cont get-curr-node-nr) 0))
             (let ((_ret (send l-tree-cont auto-fill-children)))
               (cond ((= _ret 0) (send l-tree-cont repaint-area))
                     ((= _ret -1) (show-message "Auto fill: Cannot fill the children of the list node!" 1))
                     ((= _ret -2) (show-message "Auto fill: Cannot fill the children if one or more child already exists!" 1))
                     ((= _ret -3) (show-message "Auto fill: Node do not have specified type!" 1))
                     ((= _ret -4) (show-message "Auto fill: Node do not have any children!" 1))
                     (else (show-message "Auto fill: Unexpected error!" 1)))))
            (else
             (send l-tree-cont add-child (get-child-pos))
             (update-children-cb #f)
             (send l-tree-cont repaint-area)
             (update-views))))
    
    
    (define/private (delete-node _cascade)
      (cond ((send l-tree-cont is-curr-node-set?)
             (send l-tree-cont delete-node _cascade)
             (update-children-cb #t)
             (cond ((send l-tree-cont in-references-view?)
                    (send references-list-cont clear)
                    (send references-list-cont repaint))
                   (else
                    (update-views)))
             (clear-message-box))
            ((send l-tree-cont is-curr-ref-set?)
             (cond (_cascade
                    (show-message "This option can delete only nodes!" 1))
                   (else
                    (send l-tree-cont delete-ref)
                    (send l-tree-cont repaint-area)))
             (cond ((send l-tree-cont in-references-view?)
                    (send references-list-cont delete-selected)
                    (send references-list-cont repaint))))
            (else
             (show-message "You must select a node you want to delete!" 1))))
    
    
    (define/private (move-node _side)
      (send l-tree-cont move-node _side))
    
    
    (define/private (copy-cut _type)
      (send copy-area-cont clear-tree)
      (if (= _type 1)
          (begin
            (let ((_refresh (send l-tree-cont is-any-node-set-to-cut?)))
              (send copy-area-cont add-copied-child (send l-tree-cont set-curr-node-to-copy) 1 #t)
              (show-clipboard)
              (cond (_refresh
                     (send l-tree-cont repaint-area)))))
          (send l-tree-cont set-curr-node-to-cut))
      (send copy-area-cont repaint-area))
    
    
    (define/private (show-clipboard)
      (send opts2-panel change-children (lambda (children) (list clipboard-tab)))
      (send opts2-panel set-selection 0))
    
    
    (define/private (paste-node)
      (cond ((eq? #t (send l-tree-cont is-cut-node-set?))
             (let ((_res (send l-tree-cont paste-cutted-node (get-child-pos))))
               (cond ((= _res 1) (show-message "Moving node into itself or one of its children is forbidden!" 1))
                     (else (clear-message-box))))
             (send l-tree-cont repaint-area)
             (update-children-cb #f))
            ((send copy-area-cont is-empty?)
             (show-message "The clipboard is empty!" 1))
            ((eq? #t (send l-tree-cont is-copy-node-set?))
             (send l-tree-cont add-copied-child (send copy-area-cont set-curr-node-to-copy) (get-child-pos) #f)
             (send l-tree-cont repaint-area)
             (update-children-cb #f))
            (else
             (show-message "The clipboard is empty!" 1))))
    
    
    (define/private (type-tf-key-enter event)
      (cond ((eq? (send event get-event-type) 'text-field-enter)
             (save-node-text)
             (cond ((send l-tree-cont in-types-view?)
                    (send types-list-cont update-list)
                    (send types-list-cont repaint)
                    (send l-tree-cont repaint-area))))
            (else
             (send gui-menu set-tb-nodeText-as-modified #t))))
    
    
    (define/private (type-rgb-tf-key-enter _type _event-type)
      (cond ((eq? _event-type 'text-field)
             (let ((_val 0))
               (cond ((= _type 0) (set! _val (string->number (send tb-R get-value))))
                     ((= _type 1) (set! _val (string->number (send tb-G get-value))))
                     ((= _type 2) (set! _val (string->number (send tb-B get-value)))))
               (cond ((eq? #f _val) (set! _val 0))
                     ((> _val 255) (set! _val 255))
                     ((< _val 0) (set! _val 0)))
               (cond ((= _type 0) (send tb-R set-value (number->string _val)))
                     ((= _type 1) (send tb-G set-value (number->string _val)))
                     ((= _type 2) (send tb-B set-value (number->string _val))))
               (let ((_rgb (mlist (string->number (send tb-R get-value)) (string->number (send tb-G get-value)) (string->number (send tb-B get-value)))))
                 (send types-list-cont replace-color _rgb)
                 (update-rgb-tab-prev _rgb)
                 (send l-tree-cont repaint-area)
                 (update-types-view))))))
    
    
    (define/private (save-node-text)
      (cond ((send l-tree-cont in-names-view?)
             (let* ((_n-name (string-replace (send gui-menu get-tb-nodeText-value) " " "-"))
                    (_res (send l-tree-cont set-new-name _n-name)))
               (cond ((= _res 0)
                      (send gui-menu set-tb-nodeText-value _n-name)
                      (send gui-menu set-tb-nodeText-as-modified #f)
                      (update-names-view)
                      (clear-message-box))
                     ((= _res -1)
                      (show-message "Your must choose a node!" 1))
                     (else
                      (show-message (string-append "The name is already taken by the node number " (number->string _res)) 1)))))
            (else
             (let* ((_n-name (string-replace (send gui-menu get-tb-nodeText-value) " " "-")) (_res (send l-tree-cont set-new-type _n-name)))
               (cond ((cons? _res)
                      (show-message (car _res) (cdr _res))
                      (cond ((= (cdr _res) 0)
                             (send gui-menu set-tb-nodeText-value _n-name)
                             (send gui-menu set-tb-nodeText-as-modified #f)
                             (set! curr-nodeText-value (send gui-menu get-tb-nodeText-value)))
                            (else
                             (set-new-node-text-value curr-nodeText-value))))
                     (else
                      (clear-message-box)
                      (set-new-node-text-value ""))))
             (send gui-menu set-tb-nodeText-as-modified #f)
             (update-types-view)
             (send l-tree-cont repaint-area))))
    
    
    (define/private (show-rbg-tab)
      (send opts2-panel change-children (lambda (children) (list rgb-tab)))
      (send opts2-panel set-selection 1))
    
    
    (define/private (activate-rgb-tab _type _rgb)
      (send rgb-tab enable #t)
      (send tb-R set-value (number->string (mlist-ref _rgb 0)))
      (send tb-G set-value (number->string (mlist-ref _rgb 1)))
      (send tb-B set-value (number->string (mlist-ref _rgb 2)))
      (show-rbg-tab)
      (update-rgb-tab-prev _rgb))
    
    
    (define/private (deactivate-rgb-tab)
      (send rgb-tab enable #f)
      (send tb-R set-value "")
      (send tb-G set-value "")
      (send tb-B set-value ""))
    
    
    (define/private (check-with-spec)
      (cond ((or (null? mathing-commands-dialog) (not (send mathing-commands-dialog is-shown?)))
             (let* ((_res (send l-tree-cont create-mathing-commands)) (_type (mcar _res)) (_msg (mcdr _res)))
               (cond ((= _type 0)
                      (set! mathing-commands-dialog (create-mathing-commands-dialog frame (mcdr _msg)))
                      (set! _msg (mcar _msg))))
               (show-message _msg _type)))))
    
    
    (define/public (update-node-menu)
      (set! curr-nodeText-value (send l-tree-cont update-node-menu))
      (update-ast-rules-info (send l-tree-cont get-curr-node-type-for-ast-info)))
    
    
    (define/private (get-child-pos)
      (let ((_txt (send gui-menu get-combo-addChild-value)))
        (cond ((string=? _txt "as first") 1)
              ((string=? _txt "as last") -1)
              (else
               (set! _txt (string->number (substring _txt 6)))
               (if (number? _txt)
                   (begin
                     (+ 1 (send l-tree-cont get-node-index-by-nr _txt)))
                   -1)))))
    
    
    (define/private (update-children-cb _set-default)
      (send l-tree-cont update-children-cb _set-default))
    
    
    (define/private (update-rgb-tab-prev _rgb)
      (let* ((_rgbdc (send rgb-preview get-dc)) (_w (send rgb-preview min-client-width)) (_h (send rgb-preview min-client-height)) (_bitmap (make-bitmap _w _h)) (_dc (new bitmap-dc% [bitmap _bitmap])))
        (send _dc set-brush (make-color (mlist-ref _rgb 0) (mlist-ref _rgb 1) (mlist-ref _rgb 2)) 'solid)
        (send _dc draw-rectangle 0 0 _w _h)
        (send _rgbdc clear)
        (send _rgbdc draw-bitmap _bitmap 0 0)))


    (super-new)))

(provide RagDollWin%)


