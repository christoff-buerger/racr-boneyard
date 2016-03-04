#lang racket/gui

(require rag-doll/util)
(require racr/core)
(require compatibility/mlist)

(define my-list%
  (class object%
    ; ----- variables -----
    (init-field draw-area)
    (init-field tree-class)
    (init-field area-w)
    (init-field area-h)
    (field (clist (mlist)))
    (field (row-h 20))
    (field (marg-top 15))
    (field (marg-bottom 15))
    (field (marg-left 5))
    (field (mouseover -1))
    (field (prev_over -1))
    (field (selected -1))
    (field (start-top 0))
    (field (last-hover-check 0))
    (field (changed #t))
    (init-field win-parent)    

    (super-new)

    (define/public (clear-mouseover) (set! mouseover -1))
    (define/public (clear-selection) (set! selected -1))


    (define/public (update-size)
      (define-values (_w _h) (send (send draw-area get-dc) get-size))
      (set! area-w _w)
      (set! area-h _h))

    
    (define/public (_select _index)
      (cond ((not (= selected _index))
             (let ((_n (mlength clist)))
               (if (< _index _n)
                   (set! selected _index)
                   (set! selected (- _n 1))))
             #t)
            (else
             #f)))

    
    (define/public (_check-hover _y)
      ;(let ((_time (current-inexact-milliseconds)))
        ;(cond ((>= (- _time last-hover-check) 100)
               ;(set! last-hover-check _time)
               (set! prev_over mouseover)
               (cond ((or (<= _y marg-top) (>= _y (- area-h marg-bottom)))
                      (set! mouseover -1))
                     (else
                      (let ((_over (- (inexact->exact (round (/ (- (- _y start-top) (* row-h 0.3)) row-h))) 1)))
                        (set! changed (not (= _over mouseover)))
                        (cond (changed
                               (set! mouseover _over)
                               #f);#t)
                              (else #f))))));)))
    
    
    (define/public (print-arrows _areadc _maxtop)
      (let ((_left (inexact->exact (round (- (/ area-w 2) 8)))))
          (let* ((_bitmap (make-bitmap area-w marg-top)) (_dc (new bitmap-dc% [bitmap _bitmap])))
            (send _dc set-brush (make-color 250 250 250) 'solid)
            (send _dc draw-rectangle 0 0 area-w marg-top)
            (send _dc set-brush (make-color 0 0 0) 'solid)
            (send _dc draw-polygon (list (cons 8 0) (cons 0 6) (cons 16 6)) _left 3)
            (send _areadc draw-bitmap _bitmap 0 0))
          (let* ((_bitmap (make-bitmap area-w marg-bottom)) (_dc (new bitmap-dc% [bitmap _bitmap])))
            (send _dc set-brush (make-color 250 250 250) 'solid)
            (send _dc draw-rectangle 0 0 area-w marg-bottom)
            (send _dc set-brush (make-color 0 0 0) 'solid)
            (send _dc draw-polygon (list (cons 8 6) (cons 0 0) (cons 16 0)) _left 4)
            (send _areadc draw-bitmap _bitmap 0 _maxtop))))
    
    
    (define/public (_clicked _y)
      (let ((_mintop (- area-h (+ (+ (* (mlength clist) row-h) marg-bottom) marg-top))))
        (cond ((> _mintop 0) (set! _mintop 0)))
        (cond ((< _y marg-top)
               (set! start-top (+ start-top 20))
               (cond ((> start-top 0) (set! start-top 0)))
               1)
              ((> _y (- area-h marg-bottom))
               (set! start-top (- start-top 20))
               (cond ((< start-top _mintop) (set! start-top _mintop)))
               1)
              (else
               (cond ((or (> mouseover (- (mlength clist) 1)) (< mouseover 0))
                      (set! changed (not (= selected -1)))
                      (set! selected -1)
                      null)
                     (else
                      (set! changed (not (= selected mouseover)))
                      (set! selected mouseover)
                      (mlist-ref clist mouseover)))))))
    
    
    (define/public (clear-list)
      (set! clist '()))
    
    
    (define/public (delete-selected)
      (cond ((> selected -1)
             (let ((cnt -1) (ret (mlist)))
               (mfor-each
                (lambda (_v)
                  (set! cnt (+ cnt 1))
                  (cond ((not (= selected cnt)) (set! ret (mappend ret (mlist _v))))))
                clist)
               (set! clist ret)
               (clear-selection))
             #t)
            (else
             #f)))
    
    
    (define/public (qsort-cons l)
      (if (empty? l)
          empty
          (mappend
           (qsort-cons (filter (lambda (x) (<= (mcar x) (mcar (first l)))) (rest l)))
           (mlist (first l))
           (qsort-cons (filter (lambda (x) (> (mcar x) (mcar (first l)))) (rest l))))))


    (define/public (qsort-list l pos)
      (cond ((empty? l)
             empty)
            (else
             (cond ((mlist? l) (set! l (mlist->list l))))
             (mappend
              (qsort-list (filter (lambda (x) (<= (mlist-ref x pos) (mlist-ref (first l) pos))) (rest l)) pos)
              (mlist (first l))
              (qsort-list (filter (lambda (x) (> (mlist-ref x pos) (mlist-ref (first l) pos))) (rest l)) pos)))))


))

(provide my-list%)
