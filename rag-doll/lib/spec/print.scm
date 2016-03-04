#!r6rs

(library
 (rag-doll base rules-print)
 (export
  specify-rules-print)
 (import (rnrs) (racr core) (rag-doll util) (rnrs r5rs))


(define specify-rules-print
  (lambda (spec)
    (with-specification
     spec 
     
     (ag-rule
      leaf-count
      (Node
       (lambda (n)
         (if (= (ast-num-children (ast-child 'Node* n)) 0)
             1
             (let ((sum 0))
               (ast-for-each-child
                (lambda (i child)
                  (set! sum (+ sum (att-value 'leaf-count child))))
                (ast-child 'Node* n))
               sum)))))
     
     (ag-rule
      range-restriction
      
      ((Tree Node)
       (lambda (t)
         (cons 0 (att-value 'leaf-count t))))
      
      ((Node Node*)
       (lambda (n)
         (let* ((left-bound
                 (if (= (ast-child-index n) 1)
                     (car (att-value 'range-restriction (ast-parent n)))
                     (cdr (att-value 'range-restriction (ast-sibling (- (ast-child-index n) 1) n)))))
                (right-bound
                 (+ left-bound (att-value 'leaf-count n))))
           (cons left-bound right-bound)))))
     
     (ag-rule
      x
      (Node
       (lambda(n)
         (let* ((_range (att-value 'range-restriction n)))
           (inexact->exact (round (* (+ (/ (- (cdr _range) (car _range)) 2) (car _range)) (att-value 'x-space n))))))))
     
     
     (ag-rule
      y
      (Node
       (lambda(n)
         (inexact->exact (round (* (att-value 'depth n) (att-value 'y-space n)))))))
     
     
     (ag-rule
      x-space
      (Tree
       (lambda (t)
         (/ (ast-child 'areawidth t) (att-value 'leaf-count (ast-child 'Node t))))))
     
     
     (ag-rule
      y-space
      (Tree
       (lambda (t)
         (/ (ast-child 'areaheight t) (att-value 'children-level-count (ast-child 'Node t))))))
     
     
     (ag-rule
      children-level-count
      
      (Tree
       (lambda (t)
         (att-value 'children-level-count (ast-child 'Node t))))
      
      (Node
       (lambda (n)
         (if (= (ast-num-children (ast-child 'Node* n)) 0)
             1
             (let ((_max 0))
               (ast-for-each-child
                (lambda (i child)
                  (cond ((> (att-value 'children-level-count child) _max) (set! _max (att-value 'children-level-count child)))))
                (ast-child 'Node* n))
               (+ _max 1))))))
     
     
     (ag-rule
      depth
      
      ((Tree Node)
       (lambda (t)
         0))
      
      ((Node Node*)
       (lambda (n)
         (+ (att-value 'depth (ast-parent n)) 1))))
     
     
     (ag-rule
      level-count
      (Tree
       (lambda (t)
         (att-value 'depth (ast-child 'Node t)))))
     
     
     (ag-rule
      area-w
      (Tree
       (lambda (t)
         (ast-child 'areawidth t))))
     
     
     (ag-rule
      area-h
      (Tree
       (lambda (t)
         (ast-child 'areaheight t))))
     
     
     (ag-rule
      node-ray
      (Tree
       (lambda (t)
         (let ((_xr (inexact->exact (round (/ (att-value 'x-space t) 4)))) (_yr (inexact->exact (round (/ (att-value 'y-space t) 4)))))
           (if (> _xr _yr) _yr _xr)))))
     
     
     (ag-rule
      bitmap-params
      (Node
       (lambda (n)
         (let* ((_r (att-value 'node-ray n)) (_size (* _r 2)) (_penS 2) (_margin (/ _penS 2)))
           (cond ((att-value 'is-selected n) ; selected
                  (set! _penS (+ _penS 2))
                  (set! _margin (+ _margin 1))))
           (set! _size (- _size (* _margin 4)))
           (list _size _penS _margin 12 (number->string (ast-child 'nr n)) (- _r 10) (- _r 10))))))
     
     
     (ag-rule
      bottom-y-in-range
      (Tree
       (lambda (t _nr1 _nr2 _l _r)
         (let ((_max 0) (_node-h (* (att-value 'node-ray (ast-child 'Node t)) 2)))
           (let loop ((_n (ast-child 'Node t)))
             (cond ((not (or (= _nr1 (ast-child 'nr _n)) (= _nr2 (ast-child 'nr _n))))
                    (let ((_r2 (cdr (att-value 'range-restriction _n))) (_l2 (car (att-value 'range-restriction _n))))
                      (cond ((or (and (< _l2 _r) (> _r2 _l)) (= _l _l2) (= _r _r2))
                             (set! _max (max _max (+ (att-value 'y _n) _node-h)))
                             (ast-for-each-child
                              (lambda (_i _child)
                                (loop _child))
                              (ast-child 'Node* _n))))))))
           _max))))
     
     
     (ag-rule
      bottom-y
      (Node
       (lambda (n)
         (+ (att-value 'y n) (* (att-value 'node-ray n) 2)))))
     
     
     (ag-rule
      children-links
      (Node
       (lambda (n _ray)
         (let ((_ret (list)) (_children (ast-child 'Node* n)) (_x (att-value 'x n)) (_y (att-value 'y n)) (_stat '()))
           (ast-for-each-child
            (lambda (_i _child)
              (let* ((_x2 (att-value 'x _child))
                     (_y2 (att-value 'y _child))
                     (_w (abs (- _x2 _x)))
                     (_h (abs (- _y2 _y)))
                     (_sx _x)
                     (_sy _y))
                (cond ((> _x _x2) (set! _sx _x2)))
                (cond ((> _y _y2) (set! _sy _y2)))
                (cond ((< _w 2) (set! _w 2) (set! _x (+ _x 1)) (set! _x2 (+ _x2 1))))
                (cond ((< _h 2) (set! _h 2) (set! _y (+ _y 1)) (set! _y2 (+ _y2 1))))
                (cond ((att-value 'is-to-cut? n) (set! _stat 'to-cut)))
                (set! _ret (append _ret (list (list (inexact->exact (round _sx))
                                                    (inexact->exact (round _sy))
                                                    (inexact->exact (round _w))
                                                    (inexact->exact (round _h))
                                                    (inexact->exact (round (- _x _sx)))
                                                    (inexact->exact (round (- _y _sy)))
                                                    (inexact->exact (round (- _x2 _sx)))
                                                    (inexact->exact (round (- _y2 _sy)))
                                                    _stat))))))
            _children)
           _ret))))
     
     
     (ag-rule
      references
      (Node
       (lambda (n _curr-ref)
         (let ((_refs (list)) (_break-shft 6) (_x (att-value 'x n)) (_line-w 2) (_y (att-value 'y n)) (_node-refs (ast-child 'Ref* n)) (node-r (att-value 'node-ray n)))
           (cond ((= (ast-num-children _node-refs) 0) (list))
                 (else
                  (ast-for-each-child
                   (lambda (_i _ref)
                     (cond ((not (null? _curr-ref)) ;double parent work because for Ref list it doesn't see the parent's number
                            (cond ((and (eq? (ast-child 'nr (ast-parent (ast-parent _curr-ref))) (ast-child 'nr n)) (eq? (ast-child 'nr (ast-child 'node _ref)) (ast-child 'nr (ast-child 'node _curr-ref))))
                                   (set! _line-w 3))
                                  (else
                                   (set! _line-w 2)))))
                     (let* ((_node (ast-child 'node _ref)) (_ref-sections (list)) (_ex (att-value 'x _node)) (_ey (att-value 'y _node)) (_direct #f))
                       (let ((_n-right-marg (inexact->exact (round (* (cdr (att-value 'range-restriction n)) (att-value 'x-space n)))))
                             (_node-left-marg (inexact->exact (round (* (car (att-value 'range-restriction _node)) (att-value 'x-space n)))))
                             (_n-left-marg (inexact->exact (round (* (car (att-value 'range-restriction n)) (att-value 'x-space n)))))
                             (_node-right-marg (inexact->exact (round (* (cdr (att-value 'range-restriction _node)) (att-value 'x-space n))))))
                         (cond ((= _n-right-marg _node-left-marg)
                                (set! _ref-sections (append _ref-sections (Print-Ref n _node _x _y _ex _ey))))
                               ((= _n-left-marg _node-right-marg)
                                (set! _ref-sections (append _ref-sections (Print-Ref n _node _x _y _ex _ey))))
                               ((or (att-value 'is-direct-parent? n _node) (att-value 'is-direct-parent? _node n))
                                (set! _ref-sections (append _ref-sections (Print-Ref-ParentChild (att-value 'node-ray n) _x _y _ex _ey))))
                               (else
                                (set! _ref-sections (append _ref-sections (Print-Ref n _node _x _y _ex _ey))))))
                       (set! _refs (append _refs (list (list _line-w _ref-sections))))))
                   _node-refs)))
           _refs))))
     
     
     (define Print-Ref-ParentChild
       (lambda (_node-r _sx _sy _ex _ey)
         (let* ((_res (list)) (_xop1 +) (_xop2 -) (_w (- _sx _ex)) (_h (- _sy _ey)) (_arrowgrads (/ 3.14159265359 8)) (_grads (/ 3.14159265359 18)) (_alpha2 0) (_newalpha1 0) (_newalpha2 0))
           (cond ((< _sx _ex)
                  (set! _xop1 -)
                  (set! _xop2 +)))
           (let* ((_d (expt (+ (* _w _w) (* _h _h)) 0.5)) (_sin1 (/ _w _d)) (_cos1 (/ _h _d)) (_sin2 (/ _h _d)) (_cos2 (/ _w _d)))
             (set! _alpha2 (asin _sin2))
             (if (< _sy _ey)
                 (set! _newalpha1 (+ (asin _sin1) _grads))
                 (set! _newalpha1 (- (asin _sin1) _grads)))
             (if (< _sx _ex)
                 (set! _newalpha2 (+ _alpha2 _grads))
                 (set! _newalpha2 (- _alpha2 _grads)))
             (set! _sin1 (sin _newalpha1))
             (set! _cos1 (cos _newalpha1))
             (set! _sin2 (sin _newalpha2))
             (set! _cos2 (cos _newalpha2))
             (set! _sx (- _sx (inexact->exact (round (* _sin1 _node-r)))))
             (if (< _sy _ey)
                 (set! _sy (+ _sy (inexact->exact (round (* _cos1 _node-r)))))
                 (set! _sy (- _sy (inexact->exact (round (* _cos1 _node-r))))))
             (set! _ex (_xop1 _ex (inexact->exact (round (* _cos2 _node-r)))))
             (set! _ey (+ _ey (inexact->exact (round (* _sin2 _node-r)))))
             (set! _res (append _res (list (list _sx _sy _ex _ey))))
             ;arrows
             (let ((_arrlen (inexact->exact (round (/ _node-r 4)))))
               (set! _newalpha1 (- _alpha2 _arrowgrads))
               (set! _newalpha2 (+ _alpha2 _arrowgrads))
               (set! _sin1 (sin _newalpha1))
               (set! _cos1 (cos _newalpha1))
               (set! _sin2 (sin _newalpha2))
               (set! _cos2 (cos _newalpha2))
               (set! _res (append _res (list (list _ex _ey (_xop1 _ex (inexact->exact (round (* _cos1 _arrlen)))) (+ _ey (inexact->exact (round (* _sin1 _arrlen))))))))
               (set! _res (append _res (list (list _ex _ey (_xop1 _ex (inexact->exact (round (* _cos2 _arrlen)))) (+ _ey (inexact->exact (round (* _sin2 _arrlen))))))))))
           _res)))
     
     
     (define Print-Ref
       (lambda (_sn _en _sx _sy _ex _ey)
         (let* ((_parent-child? #f) (_node-r (att-value 'node-ray _sn)) (_arr-at _en) (_bop +) (_bord1op -) (_bord2op +) (_arrchange #f) (_nbottom 0) (_top-path (inexact->exact (round (/ (att-value 'y-space _sn) 2)))) (_bottom 0) (_morecurves #t) (_relativesData '()) (_arropt -) (_lineshft 3) (_ischild #f) (_border1 0) (_border2 0) (_lrange 0) (_rrange 0) (_cpyop +) (_xop1 -) (_xop2 +) (_xop3 +) (_xop4 -) (_yop1 +) (_yop2 -) (_res (list)) (_shft (inexact->exact (round (/ _node-r 4)))) (_shft2 (inexact->exact (round (/ _node-r 8)))) (_pom 0))
           (if (< _sy _ey)
               (cond ((att-value 'is-parent? _sn _en)
                      (set! _relativesData (Print-Ref-Relatives-GetBottoms _sn _en _sx _sy _ex _ey))
                      (set! _nbottom (att-value 'depth _en))
                      (set! _sy (- _sy _lineshft))
                      (set! _ey (+ _ey _lineshft))
                      (set! _parent-child? #t)))
               (cond ((att-value 'is-parent? _en _sn)
                      (set! _pom _sx) (set! _sx _ex) (set! _ex _pom)
                      (set! _pom _sy) (set! _sy _ey) (set! _ey _pom)
                      (set! _pom _sn) (set! _sn _en) (set! _en _pom)
                      (set! _sy (+ _sy _lineshft))
                      (set! _ey (- _ey _lineshft))
                      (set! _bop -)
                      (set! _bord1op +)
                      (set! _bord2op -)
                      (set! _arrchange #t)
                      (set! _relativesData (Print-Ref-Relatives-GetBottoms _sn _en _sx _sy _ex _ey))
                      (set! _nbottom (att-value 'depth _en))
                      (set! _parent-child? #t))))
           (cond ((not (null? _relativesData))
                  (cond ((or (> (list-ref _relativesData 0) (list-ref _relativesData 2)) (and (> (list-ref _relativesData 0) (list-ref _relativesData 2)) (> (list-ref _relativesData 3) (list-ref _relativesData 1)))) ;RIGHT
                         (set! _bottom (list-ref _relativesData 2))
                         (set! _lrange (cdr (att-value 'range-restriction _sn)))
                         (set! _rrange (cdr (att-value 'range-restriction _en)))
                         (set! _ex (+ _ex (* _node-r 2)))
                         (set! _arropt +)
                         (set! _xop3 -)
                         (set! _sx (+ _sx (* _node-r 2))))
                        (else ;LEFT
                         (set! _bottom (list-ref _relativesData 0))
                         (set! _lrange (car (att-value 'range-restriction _sn)))
                         (set! _rrange (car (att-value 'range-restriction _en)))
                         (set! _ex (- _ex (* _node-r 2)))
                         (set! _xop1 +)
                         (set! _xop2 -)
                         (set! _sx (- _sx (* _node-r 2)))))
                  (let ((_val _lineshft)) (if (equal? _xop2 +) (set! _xop2 -) (set! _xop2 +))))
                 (else
                  (cond ((> _sx _ex)
                         (set! _pom _sx) (set! _sx _ex) (set! _ex _pom)
                         (set! _pom _sy) (set! _sy _ey) (set! _ey _pom)
                         (set! _pom _sn) (set! _sn _en) (set! _en _pom)
                         (set! _arrchange #t)
                         (set! _arropt +)
                         (set! _lineshft -3)))
                  (set! _lrange (cdr (att-value 'range-restriction _sn)))
                  (set! _rrange (car (att-value 'range-restriction _en)))
                  (set! _bottom (att-value 'bottom-y-in-range _sn (ast-child 'nr _sn) (ast-child 'nr _en) _lrange _rrange))
                  (cond ((< _bottom _nbottom) (set! _morecurves #f)))
                  (set! _sy (+ _sy _lineshft))
                  (set! _ey (+ _ey _lineshft))))
           (set! _border1 (_bord1op (inexact->exact (round (* _lrange (att-value 'x-space _sn)))) _lineshft))
           (set! _border2 (_bord2op (inexact->exact (round (* _rrange (att-value 'x-space _sn)))) _lineshft))
           (cond ((> _lrange _lrange) (set! _pom _rrange) (set! _rrange _lrange) (set! _lrange _pom)))
           (cond ((and (not _parent-child?)
                       (not (= (cdr (att-value 'range-restriction _sn)) (car (att-value 'range-restriction _en))))
                       (> (abs (- _sy _bottom)) (abs (- _sy _top-path)))
                       (att-value 'on-uparent-edge _sn)
                       (att-value 'on-uparent-edge _en))
                  (cond ((att-value 'on-uparent-edge _sn)
                         (cond ((and (att-value 'on-uparent-left-edge _sn))
                                (set! _sx (- _sx _node-r))
                                (cond ((< _sx _border1)
                                       (set! _border1 (_bord1op (inexact->exact (round (* (car (att-value 'range-restriction _sn)) (att-value 'x-space _sn)))) _lineshft))
                                       (set! _xop1 +)
                                       (cond (_arrchange
                                              (set! _arropt -))))))
                               ((and (att-value 'on-uparent-right-edge _sn))
                                (set! _sx (+ _sx _node-r))))))
                  (cond ((att-value 'on-uparent-edge _en)
                         (cond ((and (att-value 'on-uparent-left-edge _en))
                                (set! _ex (- _ex _node-r 2))
                                (cond ((< _ex _border2)
                                       (set! _border2 (+ (inexact->exact (round (* (car (att-value 'range-restriction _en)) (att-value 'x-space _en)))) _lineshft)))))
                               ((and (att-value 'on-uparent-right-edge _en))
                                (set! _ex (+ _ex _node-r 2))
                                (cond ((> _ex _border2)
                                       (set! _border2 (- (inexact->exact (round (* (cdr (att-value 'range-restriction _en)) (att-value 'x-space _en)))) _lineshft))
                                       (set! _xop3 -)
                                       (cond ((not _arrchange) (set! _arropt +)))))))))
                  (set! _yop1 -)
                  (set! _yop2 +)
                  (set! _bottom _top-path))
                 (else
                  (cond ((> _sy _bottom) (set! _bottom _sy) (set! _morecurves #f)))
                  (cond ((> _ey _bottom) (set! _bottom _ey) (set! _morecurves #f)))
                  (set! _sx (_xop2 _sx _node-r))
                  (set! _ex (_xop1 _ex _node-r))))
           (let ((_tmpx (- (att-value 'area-w _sn) _shft)))
             (cond ((< _border1 _shft) (set! _border1 _shft)))
             (cond ((> _border2 _tmpx) (set! _border2 _tmpx))))
           (let ((_tmpx (- (att-value 'area-h _sn) _shft)))
             (cond ((> _bottom _tmpx) (set! _bottom _tmpx))))
           (cond ((and (not (= _ey _bottom)) (not (= _sy _bottom))) (set! _bottom (_bop _bottom _lineshft))))
           (let* ((_x1 _sx) (_y1 _sy) (_x2 (_xop1 _border1 _shft)) (_y2 _sy))
             (cond ((not (= _bottom _sy))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _y1 _y2)
                    (set! _x2 _border1)
                    (set! _y2 (_yop1 _y1 _shft))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _y1 _y2)
                    (set! _y2 (_yop2 _bottom _shft))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _y1 _y2)
                    (set! _x2 (_xop2 _x1 _shft))
                    (set! _y2 _bottom)
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _y1 _y2)))
             (cond ((not (= _bottom _ey))
                    (set! _x2 (_xop4 _border2 _shft))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _x2 _border2)
                    (set! _y2 (_yop2 _bottom _shft))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _y1 _y2)
                    (set! _y2 (_yop1 _ey _shft))
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _y1 _y2)
                    (set! _x2 (_xop3 _border2 _shft))
                    (set! _y2 _ey)
                    (set! _res (append _res (list (list _x1 _y1 _x2 _y2))))
                    (set! _x1 _x2)
                    (set! _y1 _y2)))
             (set! _x2 _ex)
             (set! _res (append _res (list (list _x1 _y1 _x2 _y2)))))
           ;arrow
           (let ((_ax _ex) (_ay _ey))
             (cond (_arrchange (set! _ax _sx) (set! _ay _sy)))
             (set! _res (append _res (list (list _ax _ay (_arropt _ax _shft2) (+ _ay _shft2)))))
             (set! _res (append _res (list (list _ax _ay (_arropt _ax _shft2) (- _ay _shft2))))))
           _res)))
     
     
     (define Print-Ref-Relatives-GetBottoms
       (lambda (_sn _en _sx _sy _ex _ey)
         (let* ((_childnr (ast-child 'nr _en)) (_res (list)) (_depth 0) (_cnt 0) (_node-h (* (att-value 'node-ray _sn) 2)))
           (let loop ((_n _sn))
             (cond ((= (ast-child 'nr _n) _childnr)
                    (set! _res (append _res (list (+ _depth _node-h) _cnt)))
                    (set! _depth 0)
                    (set! _cnt 0))
                   (else
                    (let ((_children (ast-child 'Node* _n)))
                      (cond ((= (ast-num-children _children) 0)
                             (set! _cnt (+ _cnt 1))
                             (set! _depth (max _depth (att-value 'y _n))))
                            (else
                             (ast-for-each-child
                              (lambda (_i _child)
                                (loop _child))
                              _children)))))))
           (set! _res (append _res (list (+ _depth _node-h) _cnt)))
           _res)))
     
     
     (ag-rule ;for printing references which are going over the graphs
      on-uparent-left-edge
      (Node
       (lambda (n)
         (let ((_uparent-range (att-value 'range-restriction (att-value 'root-it-belongs-to n))) (_node-range (att-value 'range-restriction n)))
           (= (car _node-range) (car _uparent-range))))))
     
     
     (ag-rule ;for printing references which are going over the graphs
      on-uparent-right-edge
      (Node
       (lambda (n)
         (let ((_uparent-range (att-value 'range-restriction (att-value 'root-it-belongs-to n))) (_node-range (att-value 'range-restriction n)))
           (= (cdr _node-range) (cdr _uparent-range))))))
     
     
     (ag-rule ;for printing references which are going over the graphs
      on-uparent-edge
      (Node
       (lambda (n)
         (or (att-value 'on-uparent-left-edge n) (att-value 'on-uparent-right-edge n)))))))))
