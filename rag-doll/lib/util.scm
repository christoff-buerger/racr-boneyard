#!r6rs

(library
 (rag-doll util)
 (export
  get-pure-type
  list-contains
  list-delete-number
  list-delete-pos
  list-delete-value
  list-set!
  list-subtract
  list-union
  string-list-pos
  string-pos
  qsort-cons
  qsort-string
  qsort-string-no-list)
 (import (rnrs))
 
 
 (define (get-pure-type _type)
   (let ((len (string-length _type)))
        (cond ((> len 2)
               (let ((_pre (substring _type 0 3)))
                 (cond ((or (string=? _pre "T<=") (string=? _pre "T=>"))
                        (set! _type (substring _type 3)))
                       (else
                        (set! _pre (substring _type 0 2))
                        (cond ((or (string=? _pre "T<") (string=? _pre "T>"))
                               (set! _type (substring _type 2))))))))))
   _type)
 
 
 (define (list-delete-pos _list i)
  (let ((cnt -1) (ret '()))
    (for-each
     (lambda (_v)
       (set! cnt (+ cnt 1))
       (cond ((not(= i cnt)) (set! ret (append ret (list _v))))))
     _list)
    ret))

 
 (define (list-delete-number _list _val)
  (let ((cnt 0) (ret '()))
    (for-each
     (lambda (_item)
       (cond ((not(= _val _item)) (set! ret (append ret (list _item)))))
       (set! cnt (+ cnt 1))
       )
     _list)
    ret))


 (define (list-delete-value _list _val)
  (let ((cnt 0) (ret '()))
    (for-each
     (lambda (_item)
       (cond ((not (eq? _val _item)) (set! ret (append ret (list _item)))))
       (set! cnt (+ cnt 1))
       )
     _list)
    ret))


 (define (string-list-pos _list _val)
   (let loop ((_l _list) (_i 0))
     (if (null? _l)
         -1
         (cond ((string=? (car _l) _val) _i)
               (else                     (loop (cdr _l) (+ _i 1)))))))

 
 (define (list-contains _list _val)
  (let ((_res #f) (_i 0) (_eq? #f))
    (let loop ()
      (cond ((> (length _list) 0)
             (cond ((and (string? _val) (string? (car _list)))  (set! _eq? (string=? _val (car _list))))
                   (else                                        (set! _eq? (equal? _val (car _list)))))
             (if _eq?
                 (set! _res #t)
                 (begin
                   (set! _i (+ _i 1))
                   (set! _list (cdr _list))
                   (loop))))))
    _res))

 
 (define (list-set! _list i val)
  (let ((cnt -1) (_res '()))
    (for-each
     (lambda (v)
       (set! cnt (+ cnt 1))
       (if (= i cnt)
           (set! _res (append _res (list val)))
           (set! _res (append _res (list v)))))
     _list)
    _res))


 (define list-union
  (lambda (l1 l2)
    (let loop ((output l1) (input l2))
      (if (null? input)
          output
          (if (list-contains output (car input))
              (loop output (cdr input))
              (loop (append output (list (car input))) (cdr input)))))))
 
 
 (define list-subtract
  (lambda (l1 l2)
    (let loop ((output '()) (input l1))
      (if (null? input)
          output
          (if (list-contains l2 (car input))
              (loop output (cdr input))
              (loop (append output (list (car input))) (cdr input)))))))
 
 
 (define (string-pos str ch)
  (let ((_len (string-length str)) (_i 0))
    (let loop ()
      (if (< _i _len)
          (begin
            (cond ((char=? ch (string-ref str _i)) _i)
                  (else
                   (set! _i (+ _i 1))
                   (loop))))
          -1))))

 (define qsort-cons
   (lambda (l)
     (if (null? l)
         '()
         (append
          (qsort-cons (filter (lambda (x) (<= (car x) (car (car l)))) (cdr l)))
          (list (car l))
          (qsort-cons (filter (lambda (x) (> (car x) (car (car l)))) (cdr l)))))))


 (define qsort-string
   (lambda (l)
     (if (null? l)
         '()
         (append
          (qsort-string (filter (lambda (x) (string-ci<=? (car x) (car (car l)))) (cdr l)))
          (list (car l))
          (qsort-string (filter (lambda (x) (string-ci>? (car x) (car (car l)))) (cdr l)))))))


 (define qsort-string-no-list
   (lambda (l)
     (if (null? l)
         '()
         (append
          (qsort-string-no-list (filter (lambda (x) (string-ci<=? x (car l))) (cdr l)))
          (list (car l))
          (qsort-string-no-list (filter (lambda (x) (string-ci>? x (car l))) (cdr l)))))))


)
