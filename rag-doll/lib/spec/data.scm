#!r6rs

(library
 (rag-doll base rules-data)
 (export
  specify-rules-data)
 (import (rnrs) (racr core) (rag-doll util) (rnrs r5rs))


(define specify-rules-data
  (lambda (spec)
    (with-specification
     spec
     

 (ag-rule
  user-spec
  (Tree
   (lambda (t)
     (ast-child 'userspec t))))
 
 
 (ag-rule
  main-node-nr
  (Tree
   (lambda (t)
     (let ((_n (ast-child 'mainnode t)))
       (if (null? _n)
           -1
           (ast-child 'nr _n))))))
 
 
 (ag-rule
  get-main-node
  (Tree
   (lambda (t)
     (let ((res #f))
       (let loop ((_n (ast-child 'Node t)))
         (ast-find-child
          (lambda (i _child)
            (if (att-value 'is-main? _child)
                (set! res _child)
                (loop _child)))
          (ast-child 'Node* _n)))
       res))))

 
  (ag-rule
  get-node
  (Tree
   (lambda (t _number)
     (cond ((pair? _number) (set! _number (car _number))))
     (let ((res #f))
       (let loop ((_n (ast-child 'Node t)))
         (ast-find-child
          (lambda (i _child)
            (if (= (ast-child 'nr _child) _number)
                (set! res _child)
                (loop _child)))
          (ast-child 'Node* _n)))
       res))))
 
 
 (ag-rule 
  root-node
  ((Tree Node)
   (lambda (n)
     n)))
  
 (ag-rule 
  graph-root-node
  (Node
   (lambda (n)
     (let ((_parent (ast-parent (ast-parent n))))
       (if (= (ast-child 'nr _parent) 0)
           n
           (att-value 'graph-root-node _parent))))))
 
 
 (ag-rule
  get-all-root-nodes-nums
  (Tree
   (lambda (t)
     (let ((_ret '()) (_list (ast-child 'Node* (ast-child 'Node t))))
          (ast-for-each-child
           (lambda (_i _child)
             (set! _ret (append _ret (list (ast-child 'nr _child)))))
           _list)
       _ret))))

 


))))
