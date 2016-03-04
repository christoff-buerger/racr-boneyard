#lang racket/gui

(require racr/core)
(require rag-doll/util)
(require rag-doll/base)
(require rag-doll/lib/gui/lists/list-class)
(require compatibility/mlist)

(define MyReferencesList%
  (class my-list%
    ; ----- variables -----
    (inherit-field draw-area)
    (inherit-field tree-class)
    (inherit-field area-w)
    (inherit-field area-h)
    (inherit-field clist)
    (inherit-field row-h)
    (inherit-field marg-top)
    (inherit-field marg-bottom)
    (inherit-field marg-left)
    (inherit-field prev_over)
    (inherit-field mouseover)
    (inherit-field selected)
    (inherit-field start-top)
    (inherit-field changed)
    (inherit-field win-parent)
    
    (inherit _check-hover)
    (inherit _clicked)
    (inherit clear-selection)
    (inherit update-size)
    (inherit print-arrows)
    (inherit delete-selected)
    (inherit qsort-list)
    
    
    (define/public (clear)
      (update-list null))
    
    
    (define/public (clicked _y)
      (let ((_res (_clicked _y)))
        (cond ((and (number? _res) (= _res 0)) (set! _res '())))
        (cond ((not (null? _res))
              ; (cond ((not (= last-clicked (mlist-ref _res 1))
               (set! _res (mlist-ref _res 2))
               (cond ((ast-node? _res)
                      (send tree-class set-ref-as-curr _res)
                      (send win-parent update-node-menu)
                      ;(send tree-class update-node-menu)
                      )))
              (else
               (cond ((> selected -1)
                      (send tree-class set-ref-as-curr '())))
               (send win-parent update-node-menu)))
        (cond (changed (repaint)))
        _res))
    
    
    (define/public (check-hover _y)
      (let ((_res (_check-hover _y)))
        (cond ((and (not (null? _res)) (not (= prev_over mouseover)))
               (repaint)))
        _res))
    
    
    (define/public (update-list _clicked-refs)
      (cond ((not (null? _clicked-refs))
             (show-clicked-refs-list (convert-refs-list _clicked-refs)))
            (else
             (let ((_curr-node (send tree-class get-curr-node)))
               (cond ((> (ast-child 'nr _curr-node) 0)
                      (show-node-refs _curr-node))
                     (else ;show all
                      (let ((_list (mlist)))
                        
                        
;                        (let loop ((_n (send tree-class get-root-node)))
;                          (let ((_refs (rag-doll_get-node-references _n)) (_children (rag-doll_get-node-children _n)) (_nr (ast-child 'nr _n)))
;                            (cond ((> (ast-num-children _refs) 0)
;                                   (ast-for-each-child
;                                    (lambda (_i _ref)
;                                      (set! _list (mappend _list (mlist _ref))))
;                                    _refs)))
;                            (cond ((> (ast-num-children _children) 0)
;                                   (ast-for-each-child
;                                    (lambda (_i _child)
;                                      (loop _child))
;                                    _children)))))
                        
                        
                        (let loop ((_n (send tree-class get-root-node)))
                          (set! _list (mappend _list (get-node-refs _n)))
                          (let ((_children (rag-doll_get-node-children _n)))
                            (cond ((> (ast-num-children _children) 0)
                                   (ast-for-each-child
                                    (lambda (_i _child)
                                      (loop _child))
                                    _children)))))
                        
                        
                        (show-clicked-refs-list _list)
                        )
                      
                      )))
             (clear-selection)))
      (repaint))
    

    (define/private (get-ref-print-data _ref)
;      (string-append (number->string (ast-child 'nr (ast-parent (ast-parent _ref)))) " ---> " (number->string (ast-child 'nr (ast-child 'node _ref))))
      (string-append (number->string (mlist-ref _ref 0)) " ---> " (number->string (mlist-ref _ref 1)))
      )
    
    
    (define/private (convert-refs-list _l)
      (if (empty? _l)
          '()
          (mappend
           (mlist (mlist (ast-child 'nr (ast-parent (ast-parent (mcar _l)))) (ast-child 'nr (ast-child 'node (mcar _l))) (mcar _l)))
           (convert-refs-list (mcdr _l)))))
    
    
    (define/private (show-clicked-refs-list _refs)
      (cond ((> (mlength _refs) 0)
             (set! clist _refs)
             (set! selected 0))
            (else
             (set! clist null)
             (set! selected -1))))
    
    
    (define/private (show-clicked-refs _refs) ; to make mlist from ast-children
      (let ((_list (mlist)))
        (cond ((> (ast-num-children _refs) 0)
               (ast-for-each-child
                (lambda (_i _ref)
                  (set! _list (mappend _list (mlist _ref))))
                _refs)))
        (set! clist _list)))
    
    
    (define/private (show-node-refs _node)
      (set! clist (get-node-refs _node)))
    
    
    (define/private (get-node-refs _node)
      (qsort-list (convert-refs-list (rag-doll_get-node-references-as-list _node)) 1))
    
    
    (define/public (repaint)
      (let* ((_top (+ marg-top start-top)) (_pos 0) (_areadc (send draw-area get-dc)) (_maxtop (- area-h marg-bottom)) (_bitmapw (- area-w 20)))
        (send (send draw-area get-dc) clear)
        (cond ((null? clist) (set! clist (mlist 0))))
        (mfor-each
         (lambda (_item)
           (cond ((< _top _maxtop)
                  (cond ((or (= _pos mouseover) (= _pos selected))
                         (let* ((_bitmap (make-bitmap area-w row-h)) (_dc (new bitmap-dc% [bitmap _bitmap])))
                           (cond ((= _pos selected) (send _dc set-brush (make-color 220 220 220) 'solid))
                                 ((= _pos mouseover) (send _dc set-brush (make-color 240 240 240) 'solid)))
                           (send _dc draw-rectangle 0 0 area-w row-h)
                           (send _areadc draw-bitmap _bitmap 0 _top))))
                  (if (number? _item)
                      (send _areadc draw-text "[no references]" marg-left _top)
                      (send _areadc draw-text (get-ref-print-data _item) marg-left _top))
                  (set! _top (+ _top row-h))
                  (set! _pos (+ _pos 1)))))
         clist)
        ;arrows
        (print-arrows _areadc _maxtop)))
    
    
    (define/public (replace-color _rgb)
      (cond ((> selected -1)
             (let ((_vec (mlist-ref clist selected)))
               (set! _vec (list-set! _vec 1 _rgb))
               (set! clist (list-set! clist selected _vec))
               (send tree-class replace-types-colors clist)
               (repaint)))))
    
    
    (define/public (delete-curr)
      (cond ((and (> selected -1))
             (let ((_type (mlist-ref (mlist-ref clist selected) 0)))
               (cond ((not (string=? _type ""))
                      (send tree-class clear-graph-from-type _type)
                      (set! clist (list-delete-pos clist selected))
                      (send tree-class replace-types-colors clist)
                      (cond ((>= selected (mlength clist)) (set! selected -1)))
                      (repaint)))))))
      
    
    (super-new)))

(provide MyReferencesList%)
