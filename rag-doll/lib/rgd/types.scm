#!r6rs

(library
 (rag-doll base types)
 (export
  make-rag-doll-config
  rag-doll-config-inner-spec
  rag-doll-config-last-nr
  rag-doll-config-tree
  rag-doll-config-tree-set!
  rag-doll-config-last-nr-set!)
 (import (rnrs) (racr core))
 
 
 (define-record-type rag-doll-config
   (fields
    inner-spec
    (mutable tree)
    (mutable last-nr))))
