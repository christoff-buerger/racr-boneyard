#lang racket/gui

(require racr/core)
(require rag-doll/util)
(require rag-doll/lib/gui/lists/list-class)
(require compatibility/mlist)

(define MyTypesList%
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
    
    (inherit _check-hover)
    (inherit _clicked)
    (inherit clear-selection)
    (inherit update-size)
    (inherit print-arrows)
    (inherit _select)
    

    (define/public (select-type _type)
      (set! _type (get-pure-type _type))
      (cond ((send tree-class is-curr-node-list-node?)
             (set! _type "*")))
      (let loop ((_list clist) (_i 0))
        (if (null? _list)
            null
            (cond ((string=? (mcar (mlist-ref _list 0)) _type)
                   (set! selected _i)
                   (repaint)
                   (mlist-ref clist selected))
                  (else
                   (loop (mcdr _list) (+ _i 1)))))))
    
    
    (define/public (clicked _y)
      (let ((_res (_clicked _y)))
        ;(cond ((not (null? _res)) (repaint)))
        (cond (changed (repaint)))
        _res))
    
    
    (define/public (check-hover _y) (let ((_res (_check-hover _y))) (cond ((and (not (null? _res)) (not (= prev_over mouseover))) (repaint))) _res))

    
    (define/public (update-list)
      (set! clist (send tree-class get-types-list)))

    
    (define/public (repaint)
      (let* ((_top (+ marg-top start-top)) (_pos 0) (_areadc (send draw-area get-dc)) (_maxtop (- area-h marg-bottom)) (_bitmapw (- area-w 20)))
        (send (send draw-area get-dc) clear)
           (call/cc
            (lambda (break)
              (mfor-each
               (lambda (_item)
                 (cond ((< _top _maxtop)
                        (cond ((or (= _pos mouseover) (= _pos selected))
                               (let* ((_bitmap (make-bitmap area-w row-h)) (_dc (new bitmap-dc% [bitmap _bitmap])))
                                 (cond ((= _pos selected) (send _dc set-brush (make-color 220 220 220) 'solid))
                                       ((= _pos mouseover) (send _dc set-brush (make-color 240 240 240) 'solid)))
                                 (send _dc draw-rectangle 0 0 area-w row-h)
                                 (send _areadc draw-bitmap _bitmap 0 _top))))
                        (if (string=? "" (mlist-ref _item 0))
                            (send _areadc draw-text "[no type]" marg-left _top)
                            (send _areadc draw-text (mlist-ref _item 0) marg-left _top))
                        (let* ((_bitmap (make-bitmap 15 15)) (_dc (new bitmap-dc% [bitmap _bitmap])) (_rgb (mlist-ref _item 1)))
                          (send _dc set-brush (make-color (mlist-ref _rgb 0) (mlist-ref _rgb 1) (mlist-ref _rgb 2)) 'solid)
                          (send _dc draw-rectangle 0 0 15 15)
                                                 (send _areadc draw-bitmap _bitmap _bitmapw (+ _top 3)))
                        (set! _top (+ _top row-h))
                        (set! _pos (+ _pos 1)))
                       (else
                        (break))))
               clist)))
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
               (cond ((and (not (string=? _type "")) (send tree-class delete-type-from-list _type))
                      (set! clist (list-delete-pos clist selected))
                      (send tree-class replace-types-colors clist)
                      (send tree-class repaint-area)
                      (cond ((>= selected (mlength clist)) (set! selected -1)))
                      (repaint)))))))


    (super-new)))

(provide MyTypesList%)
