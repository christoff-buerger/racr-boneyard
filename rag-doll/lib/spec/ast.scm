#!r6rs

(library
 (rag-doll base ast)
 (export
  specify-ast)
 (import (rnrs) (racr core))

 (define specify-ast
   (lambda (spec)
     (with-specification
      spec
      (ast-rule 'Tree->Node-areawidth-areaheight-userspec-currnode-currref-cutnode-copynode-mainnode)
      (ast-rule 'Node->Node*-nr-type-status-mockup-Ref*-name)
      (ast-rule 'Ref->node-name))))
)
