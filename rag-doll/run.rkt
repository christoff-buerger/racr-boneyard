#lang racket/gui

(require rag-doll/main)
(require composed-petrinets/user-interface)
(require composed-petrinets/analyses)

(initialise-petrinet-language)
(define rag-doll (new RagDoll%))
(send rag-doll init pn)