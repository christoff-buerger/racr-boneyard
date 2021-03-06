; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

(define-library
  (racr core)

  (export
   ; Specification interface:
   (rename make-racr-specification create-specification)
   ; Specification query interface:
   specification->phase
   specification->start-symbol
   specification->ast-rules
   specification->find-ast-rule
   ast-rule->symbolic-representation
   ast-rule->supertype?
   ast-rule->production
   symbol->name
   symbol->non-terminal?
   symbol->kleene?
   symbol->context-name
   symbol->attributes
   attribute->name
   attribute->circular?
   attribute->synthesized?
   attribute->inherited?
   attribute->cached?
   ; ASTs: Specification
   (rename specify-ast-rule ast-rule)
   compile-ast-specifications
   ; ASTs: Construction
   create-ast
   create-ast-list
   create-ast-bud
   create-ast-mockup
   ; ASTs: Traversal
   ast-parent
   ast-child
   ast-sibling
   ast-children
   ast-for-each-child
   ast-find-child
   ast-find-child*
   ; ASTs: Node Information
   ast-node?
   ast-specification
   ast-has-parent?
   ast-child-index
   ast-has-child?
   ast-num-children
   ast-has-sibling?
   ast-node-type
   ast-node-rule
   ast-list-node?
   ast-bud-node?
   ast-subtype?
   ; Attribution: Specification
   specify-attribute
   (rename specify-ag-rule ag-rule)
   compile-ag-specifications
   ; Attribution: Querying
   att-value
   ; Rewriting: Primitive Rewrite Functions
   rewrite-terminal
   rewrite-refine
   rewrite-abstract
   rewrite-subtree
   rewrite-add
   rewrite-insert
   rewrite-delete
   ; Rewriting: Rewrite Strategies
   perform-rewrites
   ; Annotations: Attachment
   ast-annotation-set!
   ast-weave-annotations
   ast-annotation-remove!
   ; Annotations: Querying
   ast-annotation?
   ast-annotation
   ; Support
   with-specification
   with-bindings
   ; Utility interface:
   (rename error-object? racr-exception?))

  (import
   (scheme base)
   (scheme case-lambda)
   (scheme char)
   (scheme write)
   (rnrs lists)
   (rnrs hashtables)
   (kawa base))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internal Data Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; Constructor for unique entities internally used by the RACR system
  (define-record-type racr-nil-record (make-racr-nil-record) racr-nil-record?)
  (define racr-nil (make-racr-nil-record)) ; Unique value indicating undefined RACR entities
 
  ; Record type representing RACR compiler specifications. A compiler specification consists of arbitrary
  ; many AST rule, attribute and rewrite specifications, all aggregated into a set of rules stored in a
  ; non-terminal-symbol -> ast-rule hashtable, an actual compiler specification phase and a distinguished
  ; start symbol. The specification phase is an internal flag indicating the RACR system the compiler's
  ; specification progress. Possible phases are:
  ; 1 : AST specification
  ; 2 : AG specification
  ; 3 : Rewrite specification
  ; 4 : Specification finished
  (define-record-type racr-specification
    (make-racr-specification* specification-phase rules-table start-symbol)
    racr-specification?
    (specification-phase racr-specification-specification-phase racr-specification-specification-phase-set!)
    (rules-table racr-specification-rules-table)
    (start-symbol racr-specification-start-symbol racr-specification-start-symbol-set!))
  (define (make-racr-specification p r s)
    (make-racr-specification* 1 (make-eq-hashtable 50) racr-nil))
  
  ; INTERNAL FUNCTION: Given a RACR specification and a non-terminal, return the
  ; non-terminal's AST rule or #f if it is undefined.
  (define racr-specification-find-rule
    (lambda (spec non-terminal)
      (hashtable-ref (racr-specification-rules-table spec) non-terminal #f)))
 
  ; INTERNAL FUNCTION: Given a RACR specification return a list of its AST rules.
  (define racr-specification-rules-list
    (lambda (spec)
      (call-with-values
       (lambda ()
         (hashtable-entries (racr-specification-rules-table spec)))
       (lambda (key-vector value-vector)
         (vector->list value-vector)))))
 
  ; Record type for AST rules; An AST rule has a reference to the RACR specification it belongs to and consist
  ; of its symbolic encoding, a production (i.e., a list of production-symbols) and an optional supertype.
  (define-record-type ast-rule
    (make-ast-rule specification as-symbol production supertype?)
    ast-rule?
    (specification ast-rule-specification)
    (as-symbol ast-rule-as-symbol)
    (production ast-rule-production ast-rule-production-set!)
    (supertype? ast-rule-supertype? ast-rule-supertype?-set!))
  
  ; INTERNAL FUNCTION: Given an AST rule find a certain child context by name. If the rule defines no such
  ; context, return #f, otherwise the production symbol defining the respective context.
  (define ast-rule-find-child-context
    (lambda (r context-name)
      (find
       (lambda (symbol)
         (eq? (symbol-context-name symbol) context-name))
       (cdr (ast-rule-production r)))))
 
  ; INTERNAL FUNCTION: Given two rules r1 and r2, return whether r1 is a subtype of r2 or not. The subtype
  ; relationship is reflexive, i.e., every type is a subtype of itself.
  ; BEWARE: Only works correct if supertypes are resolved, otherwise an exception can be thrown!
  (define ast-rule-subtype?
    (lambda (r1 r2)
      (and
       (eq? (ast-rule-specification r1) (ast-rule-specification r2))
       (let loop ((r1 r1))
         (cond
           ((eq? r1 r2) #t)
           ((ast-rule-supertype? r1) (loop (ast-rule-supertype? r1)))
           (else #f))))))
 
  ; INTERNAL FUNCTION: Given a rule, return a list containing all its subtypes except the rule itself.
  ; BEWARE: Only works correct if supertypes are resolved, otherwise an exception can be thrown!
  (define ast-rule-subtypes
    (lambda (rule1)
      (filter
       (lambda (rule2)
         (and (not (eq? rule2 rule1)) (ast-rule-subtype? rule2 rule1)))
       (racr-specification-rules-list (ast-rule-specification rule1)))))
 
  ; Record type for production symbols; A production symbol is part of a certain ast rule and has name,
  ; a flag indicating whether it is a non-terminal or not (later resolved to the actual AST rule representing
  ; the respective non-terminal), a flag indicating whether it represents a Kleene closure (i.e., is a list
  ; of certain type) or not, a context-name unambiguously referencing it within the production it is part of
  ; and a list of attributes defined for it.
  (define-record-type symbol
    (make-production-symbol name ast-rule non-terminal? kleene? context-name attributes)
    production-symbol?
    (name symbol-name)
    (ast-rule symbol-ast-rule)
    (non-terminal? symbol-non-terminal? symbol-non-terminal?-set!)
    (kleene? symbol-kleene?)
    (context-name symbol-context-name)
    (attributes symbol-attributes symbol-attributes-set!))
  
  ; Record type for attribute definitions. An attribute definition has a certain name, a definition context
  ; (i.e., a symbol of an AST rule), an equation and an optional circularity-definition used for fix-point
  ; computations. Further, attribute definitions specify whether the value of instances of the defined
  ; attribute are cached. Circularity-definitions are (bottom-value equivalence-function) pairs, whereby
  ; bottom-value is the value fix-point computations start with and equivalence-functions are used to decide
  ; whether a fix-point is reached or not (i.e., equivalence-functions are arbitrary functions of arity two
  ; computing whether two given arguments are equal or not).
  (define-record-type attribute-definition
    (make-attribute-definition name context equation circularity-definition cached?)
    attribute-definition?
    (name attribute-definition-name)
    (context attribute-definition-context)
    (equation attribute-definition-equation)
    (circularity-definition attribute-definition-circularity-definition)
    (cached? attribute-definition-cached?))
  
  ; INTERNAL FUNCTION: Given an attribute definition, check if instances can depend on
  ; themself (i.e., be circular) or not.
  (define attribute-definition-circular?
    (lambda (att)
      (if (attribute-definition-circularity-definition att) #t #f)))
 
  ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies
  ; a synthesized attribute or not.
  (define attribute-definition-synthesized?
    (lambda (att-def)
      (let ((symbol (attribute-definition-context att-def)))
        (eq? (car (ast-rule-production (symbol-ast-rule symbol))) symbol))))
 
  ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies
  ; an inherited attribute or not.
  (define attribute-definition-inherited?
    (lambda (att-def)
      (not (attribute-definition-synthesized? att-def))))
 
  ; Record type for AST nodes. AST nodes have a reference to the evaluator state used for evaluating their
  ; attributes and rewrites, the AST rule they represent a context of, their parent, children, attribute
  ; instances, attribute cache entries they influence and annotations.
  (define-record-type node
    (make-node* evaluator-state ast-rule parent children attributes cache-influences annotations)
    node?
    (evaluator-state node-evaluator-state node-evaluator-state-set!)
    (ast-rule node-ast-rule node-ast-rule-set!)
    (parent node-parent node-parent-set!)
    (children node-children node-children-set!)
    (attributes node-attributes node-attributes-set!)
    (cache-influences node-cache-influences node-cache-influences-set!)
    (annotations node-annotations node-annotations-set!))
  (define (make-node r p c)
    (make-node* #f r p c (list) (list) (list)))
  
  ; INTERNAL FUNCTION: Given a node, return whether it is a terminal or not.
  (define node-terminal?
    (lambda (n)
      (eq? (node-ast-rule n) 'terminal)))
 
  ; INTERNAL FUNCTION: Given a node, return whether it is a non-terminal or not.
  (define node-non-terminal?
    (lambda (n)
      (not (node-terminal? n))))
 
  ; INTERNAL FUNCTION: Given a node, return whether it is a list node or not.
  (define node-list-node?
    (lambda (n)
      (eq? (node-ast-rule n) 'list-node)))
 
  ; INTERNAL FUNCTION: Given a node, return whether it is a bud node or not.
  (define node-bud-node?
    (lambda (n)
      (eq? (node-ast-rule n) 'bud-node)))
 
  ; INTERNAL FUNCTION: Given a node, return its child-index if it has a parent, otherwise return #f.
  (define node-child-index?
    (lambda (n)
      (if (node-parent n)
          (let loop ((children (node-children (node-parent n)))
                     (pos 1))
            (if (eq? (car children) n)
                pos
                (loop (cdr children) (+ pos 1))))
          #f)))
 
  ; INTERNAL FUNCTION: Given a node find a certain child by name. If the node has
  ; no such child, return #f, otherwise the child.
  (define node-find-child
    (lambda (n context-name)
      (and (not (node-list-node? n))
           (not (node-bud-node? n))
           (not (node-terminal? n))
           (let loop ((contexts (cdr (ast-rule-production (node-ast-rule n))))
                      (children (node-children n)))
             (if (null? contexts)
                 #f
                 (if (eq? (symbol-context-name (car contexts)) context-name)
                     (car children)
                     (loop (cdr contexts) (cdr children))))))))
 
  ; INTERNAL FUNCTION: Given a node find a certain attribute associated with it. If the node
  ; has no such attribute, return #f, otherwise the attribute.
  (define node-find-attribute
    (lambda (n name)
      (find
       (lambda (att)
         (eq? (attribute-definition-name (attribute-instance-definition att)) name))
       (node-attributes n))))
 
  ; INTERNAL FUNCTION: Given two nodes n1 and n2, return whether n1 is within the subtree spaned by n2 or not.
  (define node-inside-of?
    (lambda (n1 n2)
      (cond
        ((eq? n1 n2) #t)
        ((node-parent n1) (node-inside-of? (node-parent n1) n2))
        (else #f))))
 
  ; Record type for attribute instances of a certain attribute definition, associated with
  ; a certain node (context) and a cache.
  (define-record-type attribute-instance
    (make-attribute-instance* definition context cache)
    attribute-instance?
    (definition attribute-instance-definition attribute-instance-definition-set!)
    (context attribute-instance-context attribute-instance-context-set!)
    (cache attribute-instance-cache))
  (define (make-attribute-instance d c)
    (make-attribute-instance* d c (make-hashtable equal-hash equal? 1)))
  
  ; Record type for attribute cache entries. Attribute cache entries represent the values of
  ; and dependencies between attribute instances evaluated for certain arguments. The attribute
  ; instance of which an entry represents a value is called its context. If an entry already
  ; is evaluated, it caches the result of its context evaluated for its arguments. If an entry is
  ; not evaluated but its context is circular it stores an intermediate result of its fixpoint
  ; computation, called cycle value. Entries also track whether they are already in evaluation or
  ; not, such that the attribute evaluator can detect unexpected cycles.
  (define-record-type attribute-cache-entry
    (make-attribute-cache-entry* context arguments value cycle-value entered? node-dependencies cache-dependencies cache-influences)
    attribute-cache-entry?
    (context attribute-cache-entry-context attribute-cache-entry-context-set!)
    (arguments attribute-cache-entry-arguments attribute-cache-entry-arguments-set!)
    (value attribute-cache-entry-value attribute-cache-entry-value-set!)
    (cycle-value attribute-cache-entry-cycle-value attribute-cache-entry-cycle-value-set!)
    (entered? attribute-cache-entry-entered? attribute-cache-entry-entered?-set!)
    (node-dependencies attribute-cache-entry-node-dependencies attribute-cache-entry-node-dependencies-set!)
    (cache-dependencies attribute-cache-entry-cache-dependencies attribute-cache-entry-cache-dependencies-set!)
    (cache-influences attribute-cache-entry-cache-influences attribute-cache-entry-cache-influences-set!))
  (define (make-attribute-cache-entry att arg)
    (make-attribute-cache-entry*
     att
     arg
     racr-nil
     (let ((circular? (attribute-definition-circularity-definition (attribute-instance-definition att))))
       (if circular?
           (car circular?)
           racr-nil))
     #f
     (list)
     (list)
     (list)))
  
  ; Record type representing the internal state of RACR systems throughout their execution, i.e., while
  ; evaluating attributes and rewriting ASTs. An evaluator state consists of a flag indicating if the AG
  ; currently performs a fix-point evaluation, a flag indicating if throughout a fix-point iteration the
  ; value of an attribute changed and an attribute evaluation stack used for dependency tracking.
  (define-record-type evaluator-state
    (make-evaluator-state* ag-in-cycle? ag-cycle-change? evaluation-stack)
    evaluator-state?
    (ag-in-cycle? evaluator-state-ag-in-cycle? evaluator-state-ag-in-cycle?-set!)
    (ag-cycle-change? evaluator-state-ag-cycle-change? evaluator-state-ag-cycle-change?-set!)
    (evaluation-stack evaluator-state-evaluation-stack evaluator-state-evaluation-stack-set!))
  (define (make-evaluator-state)
    (make-evaluator-state* #f #f (list)))
  
  ; INTERNAL FUNCTION: Given an evaluator state, return whether it represents an evaluation in progress or
  ; not; If it represents an evaluation in progress return the current attribute in evaluation, otherwise #f.
  (define evaluator-state-in-evaluation?
    (lambda (state)
      (and (not (null? (evaluator-state-evaluation-stack state))) (car (evaluator-state-evaluation-stack state)))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; INTERNAL FUNCTION: Given an arbitrary Scheme entity, construct a string
  ; representation of it using display.
  (define object->string
    (lambda (x)
      (call-with-output-string
       (lambda (port)
         (display x port)))))
 
  ; INTERNAL FUNCTION: Given an arbitrary sequence of strings and other Scheme entities, concatenate them to
  ; form an error message and throw a special RACR exception with the constructed message. Any entity that is
  ; not a string is treated as error information embedded in the error message between [ and ] characters,
  ; whereby the actual string representation of the entity is obtained using object->string.
  (define-syntax throw-exception
    (syntax-rules ()
      ((_ m-part ...)
       (error
        (string-append
         "RACR exception: "
         (let ((m-part* m-part))
           (if (string? m-part*)
               m-part*
               (string-append "[" (object->string m-part*) "]"))) ...)))))
 
  ; INTERNAL FUNCTION: Procedure sequentially applying a function on all the AST rules of a set of rules which
  ; inherit, whereby supertypes are processed before their subtypes.
  (define apply-wrt-ast-inheritance
    (lambda (func rules)
      (let loop ((resolved ; The set of all AST rules that are already processed....
                  (filter ; ...Initially it consists of all the rules that have no supertypes.
                   (lambda (rule)
                     (not (ast-rule-supertype? rule)))
                   rules))
                 (to-check ; The set of all AST rules that still must be processed....
                  (filter ; ...Initially it consists of all the rules that have supertypes.
                   (lambda (rule)
                     (ast-rule-supertype? rule))
                   rules)))
        (let ((to-resolve ; ...Find a rule that still must be processed and...
               (find
                (lambda (rule)
                  (memq (ast-rule-supertype? rule) resolved)) ; ...whose supertype already has been processed....
                to-check)))
          (when to-resolve ; ...If such a rule exists,...
            (func to-resolve) ; ...process it and...
            (loop (cons to-resolve resolved) (remq to-resolve to-check))))))) ; ...recur.
 
  (define-syntax with-specification
    (lambda (x)
      (syntax-case x ()
        ((k spec body ...)
         #`(let* ((spec* spec)
                  (#,(datum->syntax #'k 'ast-rule)
                   (lambda (rule)
                     (specify-ast-rule spec* rule)))
                  (#,(datum->syntax #'k 'compile-ast-specifications)
                   (lambda (start-symbol)
                     (compile-ast-specifications spec* start-symbol)))
                  (#,(datum->syntax #'k 'compile-ag-specifications)
                   (lambda ()
                     (compile-ag-specifications spec*)))
                  (#,(datum->syntax #'k 'create-ast)
                   (lambda (rule children)
                     (create-ast spec* rule children)))
                  (#,(datum->syntax #'k 'specification->phase)
                   (lambda ()
                     (specification->phase spec*)))
                  (#,(datum->syntax #'k 'specify-attribute)
                   (lambda (att-name non-terminal index cached? equation circ-def)
                     (specify-attribute spec* att-name non-terminal index cached? equation circ-def))))
             (let-syntax ((#,(datum->syntax #'k 'ag-rule)
                           (syntax-rules ()
                             ((_ attribute-name definition (... ...))
                              (specify-ag-rule spec* attribute-name definition (... ...))))))
               body ...))))))
 
  (define-syntax with-bindings
    (syntax-rules ()
      ((_ ((binding ...) (parameter ...)) body body* ...)
       (lambda (l parameter ...)
         (let ((binding (cdr (assq 'binding l))) ...)
           body
           body* ...)))
      ((_ (binding ...) body body* ...)
       (with-bindings ((binding ...) ()) body body* ...))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Annotations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (define ast-weave-annotations
    (lambda (node type name value)
      (when (evaluator-state-in-evaluation? (node-evaluator-state node))
        (throw-exception
         "Cannot weave " name " annotation; "
         "There are attributes in evaluation."))
      (when (not (ast-annotation? node name))
        (cond
          ((and (not (ast-list-node? node)) (not (ast-bud-node? node)) (ast-subtype? node type))
           (ast-annotation-set! node name value))
          ((and (ast-list-node? node) (eq? type 'list-node))
           (ast-annotation-set! node name value))
          ((and (ast-bud-node? node) (eq? type 'bud-node))
           (ast-annotation-set! node name value))))
      (for-each
       (lambda (child)
         (unless (node-terminal? child)
           (ast-weave-annotations child type name value)))
       (node-children node))))
 
  (define ast-annotation?
    (lambda (node name)
      (when (evaluator-state-in-evaluation? (node-evaluator-state node))
        (throw-exception
         "Cannot check for " name " annotation; "
         "There are attributes in evaluation."))
      (assq name (node-annotations node))))
 
  (define ast-annotation
    (lambda (node name)
      (when (evaluator-state-in-evaluation? (node-evaluator-state node))
        (throw-exception
         "Cannot access " name " annotation; "
         "There are attributes in evaluation."))
      (let ((annotation (ast-annotation? node name)))
        (if annotation
            (cdr annotation)
            (throw-exception
             "Cannot access " name " annotation; "
             "The given node has no such annotation.")))))
 
  (define ast-annotation-set!
    (lambda (node name value)
      (when (evaluator-state-in-evaluation? (node-evaluator-state node))
        (throw-exception
         "Cannot set " name " annotation; "
         "There are attributes in evaluation."))
      (when (not (symbol? name))
        (throw-exception
         "Cannot set " name " annotation; "
         "Annotation names must be Scheme symbols."))
      (let ((annotation (ast-annotation? node name))
            (value
             (if (procedure? value)
                 (lambda args
                   (apply value node args))
                 value)))
        (if annotation
            (set-cdr! annotation value)
            (node-annotations-set! node (cons (cons name value) (node-annotations node)))))))
 
  (define ast-annotation-remove!
    (lambda (node name)
      (when (evaluator-state-in-evaluation? (node-evaluator-state node))
        (throw-exception
         "Cannot remove " name " annotation; "
         "There are attributes in evaluation."))
      (node-annotations-set!
       node
       (remp
        (lambda (entry)
          (eq? (car entry) name))
        (node-annotations node)))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (define specify-ast-rule
    (lambda (spec rule)
      ;;; Ensure, that the RACR system is in the correct specification phase:
      (when (> (racr-specification-specification-phase spec) 1)
        (throw-exception
         "Unexpected AST rule " rule "; "
         "AST rules can only be defined in the AST specification phase."))
      (letrec* ((ast-rule ; The parsed AST rule that will be added to the given specification.
                 (make-ast-rule
                  spec
                  rule
                  racr-nil
                  racr-nil))
                (rule-string (symbol->string rule)) ; String representation of the encoded rule (used for parsing)
                (pos 0) ; The current parsing position
                ; Support function returning, whether the end of the parsing string is reached or not:
                (eos?
                 (lambda ()
                   (= pos (string-length rule-string))))
                ; Support function returning the current character to parse:
                (my-peek-char
                 (lambda ()
                   (string-ref rule-string pos)))
                ; Support function returning the current character to parse and incrementing the parsing position:
                (my-read-char
                 (lambda ()
                   (let ((c (my-peek-char)))
                     (set! pos (+ pos 1))
                     c)))
                ; Support function matching a certain character:
                (match-char!
                 (lambda (c)
                   (if (eos?)
                       (throw-exception
                        "Unexpected end of AST rule " rule ";"
                        "Expected " c " character.")
                       (if (char=? (my-peek-char) c)
                           (set! pos (+ pos 1))
                           (throw-exception
                            "Invalid AST rule " rule "; "
                            "Unexpected " (my-peek-char) " character.")))))
                ; Support function parsing a symbol, i.e., retrieving its name, type, if it is a list and optional context name.
                (parse-symbol
                 (lambda (location) ; location: l-hand, r-hand 
                   (let ((symbol-type (if (eq? location 'l-hand) "non-terminal" "terminal")))
                     (when (eos?)
                       (throw-exception
                        "Unexpected end of AST rule " rule "; "
                        "Expected " symbol-type "."))
                     (let* ((parse-name
                             (lambda (terminal?)
                               (let ((name
                                      (append
                                       (let loop ((chars (list)))
                                         (if (and (not (eos?)) (char-alphabetic? (my-peek-char)))
                                             (begin
                                               (when (and terminal? (not (char-lower-case? (my-peek-char))))
                                                 (throw-exception
                                                  "Invalid AST rule " rule "; "
                                                  "Unexpected " (my-peek-char) " character."))
                                               (loop (cons (my-read-char) chars)))
                                             (reverse chars)))
                                       (let loop ((chars (list)))
                                         (if (and (not (eos?)) (char-numeric? (my-peek-char)))
                                             (loop (cons (my-read-char) chars))
                                             (reverse chars))))))
                                 (when (null? name)
                                   (throw-exception
                                    "Unexpected " (my-peek-char) " character in AST rule " rule "; "
                                    "Expected " symbol-type "."))
                                 (unless (char-alphabetic? (car name))
                                   (throw-exception
                                    "Malformed name in AST rule " rule "; "
                                    "Names must start with a letter."))
                                 name)))
                            (terminal? (char-lower-case? (my-peek-char)))
                            (name (parse-name terminal?))
                            (kleene?
                             (and
                              (not terminal?)
                              (eq? location 'r-hand)
                              (not (eos?))
                              (char=? (my-peek-char) #\*)
                              (my-read-char)))
                            (context-name?
                             (and
                              (not terminal?)
                              (eq? location 'r-hand)
                              (not (eos?))
                              (char=? (my-peek-char) #\<)
                              (my-read-char)
                              (parse-name #f)))
                            (name-string (list->string name))
                            (name-symbol (string->symbol name-string)))
                       (when (and terminal? (eq? location 'l-hand))
                         (throw-exception
                          "Unexpected " name " terminal in AST rule " rule "; "
                          "Left hand side symbols must be non-terminals."))
                       (make-production-symbol
                        name-symbol
                        ast-rule
                        (not terminal?)
                        kleene?
                        (if context-name?
                            (string->symbol (list->string context-name?))
                            (if kleene?
                                (string->symbol (string-append name-string "*"))
                                name-symbol))
                        (list))))))
                (l-hand (parse-symbol 'l-hand)); The rule's l-hand
                (supertype ; The rule's super-type
                 (and (not (eos?)) (char=? (my-peek-char) #\:) (my-read-char) (symbol-name (parse-symbol 'l-hand)))))
               (match-char! #\-)
               (match-char! #\>)
               (ast-rule-production-set!
                ast-rule
                (append
                 (list l-hand)
                 (let loop ((r-hand
                             (if (not (eos?))
                                 (list (parse-symbol 'r-hand))
                                 (list))))
                   (if (eos?)
                       (reverse r-hand)
                       (begin
                         (match-char! #\-)
                         (loop (cons (parse-symbol 'r-hand) r-hand)))))))
               (ast-rule-supertype?-set!
                ast-rule
                supertype)
               ; Check, that the rule's l-hand is not already defined:
               (when (racr-specification-find-rule spec (symbol-name l-hand))
                 (throw-exception
                  "Invalid AST rule " rule "; "
                  "Redefinition of " (symbol-name l-hand) "."))
               (hashtable-set! ; Add the rule to the RACR specification.
                (racr-specification-rules-table spec)
                (symbol-name l-hand)
                ast-rule))))
 
  (define compile-ast-specifications
    (lambda (spec start-symbol)
      ;;; Ensure, that the RACR system is in the correct specification phase and...
      (let ((current-phase (racr-specification-specification-phase spec)))
        (if (> current-phase 1)
            (throw-exception
             "Unexpected AST compilation; "
             "The AST specifications already have been compiled.")
            ; ...iff so proceed to the next specification phase:
            (racr-specification-specification-phase-set! spec (+ current-phase 1))))
     
      (racr-specification-start-symbol-set! spec start-symbol)
      (let* ((rules-list (racr-specification-rules-list spec))
             ; Support function, that given a rule R returns a list of all rules directly derivable from R:
             (derivable-rules
              (lambda (rule*)
                (fold-left
                 (lambda (result symb*)
                   (if (symbol-non-terminal? symb*)
                       (append result (list (symbol-non-terminal? symb*)) (ast-rule-subtypes (symbol-non-terminal? symb*)))
                       result))
                 (list)
                 (cdr (ast-rule-production rule*))))))
       
        ;;; Resolve supertypes and non-terminals occuring in productions and ensure all non-terminals are defined:
        (for-each
         (lambda (rule*)
           (when (ast-rule-supertype? rule*)
             (let ((supertype-entry (racr-specification-find-rule spec (ast-rule-supertype? rule*))))
               (if (not supertype-entry)
                   (throw-exception
                    "Invalid AST rule " (ast-rule-as-symbol rule*) "; "
                    "The supertype " (ast-rule-supertype? rule*) " is not defined.")
                   (ast-rule-supertype?-set! rule* supertype-entry))))
           (for-each
            (lambda (symb*)
              (when (symbol-non-terminal? symb*)
                (let ((symb-definition (racr-specification-find-rule spec (symbol-name symb*))))
                  (when (not symb-definition)
                    (throw-exception
                     "Invalid AST rule " (ast-rule-as-symbol rule*) "; "
                     "Non-terminal " (symbol-name symb*) " is not defined."))
                  (symbol-non-terminal?-set! symb* symb-definition))))
            (cdr (ast-rule-production rule*))))
         rules-list)
       
        ;;; Ensure, that inheritance is cycle-free:
        (for-each
         (lambda (rule*)
           (when (memq rule* (ast-rule-subtypes rule*))
             (throw-exception
              "Invalid AST grammar; "
              "The definition of " (ast-rule-as-symbol rule*) " depends on itself (cyclic inheritance).")))
         rules-list)
       
        ;;; Ensure, that the start symbol is defined:
        (unless (racr-specification-find-rule spec start-symbol)
          (throw-exception
           "Invalid AST grammar; "
           "The start symbol " start-symbol " is not defined."))
       
        ;;; Resolve inherited production symbols:
        (apply-wrt-ast-inheritance
         (lambda (rule)
           (ast-rule-production-set!
            rule
            (append
             (list (car (ast-rule-production rule)))
             (map
              (lambda (symbol)
                (make-production-symbol
                 (symbol-name symbol)
                 rule
                 (symbol-non-terminal? symbol)
                 (symbol-kleene? symbol)
                 (symbol-context-name symbol)
                 (list)))
              (cdr (ast-rule-production (ast-rule-supertype? rule))))
             (cdr (ast-rule-production rule)))))
         rules-list)
       
        ;;; Ensure context-names are unique:
        (for-each
         (lambda (ast-rule)
           (for-each
            (lambda (symbol)
              (unless (eq? (ast-rule-find-child-context ast-rule (symbol-context-name symbol)) symbol)
                (throw-exception
                 "Invalid AST grammar; "
                 "The context name " (symbol-context-name symbol) " is not unique for rule " (ast-rule-as-symbol ast-rule) ".")))
            (cdr (ast-rule-production ast-rule))))
         rules-list)
       
        ;;; Ensure, that all non-terminals can be derived from the start symbol:
        (let* ((start-rule (racr-specification-find-rule spec start-symbol))
               (to-check (cons start-rule (ast-rule-subtypes start-rule)))
               (checked (list)))
          (let loop ()
            (unless (null? to-check)
              (let ((rule* (car to-check)))
                (set! to-check (cdr to-check))
                (set! checked (cons rule* checked))
                (for-each
                 (lambda (derivable-rule)
                   (when (and
                          (not (memq derivable-rule checked))
                          (not (memq derivable-rule to-check)))
                     (set! to-check (cons derivable-rule to-check))))
                 (derivable-rules rule*))
                (loop))))
          (let ((non-derivable-rules
                 (filter
                  (lambda (rule*)
                    (not (memq rule* checked)))
                  rules-list)))
            (unless (null? non-derivable-rules)
              (throw-exception
               "Invalid AST grammar; "
               "The rules " (map ast-rule-as-symbol non-derivable-rules) " cannot be derived."))))
       
        ;;; Ensure, that all non-terminals are productive:
        (let* ((productive-rules (list))
               (to-check rules-list)
               (productive-rule?
                (lambda (rule*)
                  (not (find
                        (lambda (symb*)
                          (and
                           (symbol-non-terminal? symb*)
                           (not (symbol-kleene? symb*)) ; Unbounded repetitions are always productive because of the empty list.
                           (not (memq (symbol-non-terminal? symb*) productive-rules))))
                        (cdr (ast-rule-production rule*)))))))
          (let loop ()
            (let ((productive-rule
                   (find productive-rule? to-check)))
              (when productive-rule
                (set! to-check (remq productive-rule to-check))
                (set! productive-rules (cons productive-rule productive-rules))
                (loop))))
          (unless (null? to-check)
            (throw-exception
             "Invalid AST grammar; "
             "The rules " (map ast-rule-as-symbol to-check) " are not productive."))))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Attribute Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (define-syntax specify-ag-rule
    (syntax-rules ()
      ((_ spec att-name definition ...)
       (let ((spec* spec)
             (att-name* 'att-name))
         (specify-attribute* spec* att-name* definition) ...))))

  (define-syntax specify-attribute*
    (syntax-rules ()
      ((_ spec* att-name* ((non-terminal index) equation))
       (specify-attribute spec* att-name* 'non-terminal 'index #t equation #f))
      ((_ spec* att-name* ((non-terminal index) cached? equation))
       (specify-attribute spec* att-name* 'non-terminal 'index cached? equation #f))
      ((_ spec* att-name* ((non-terminal index) equation bottom equivalence-function))
       (specify-attribute spec* att-name* 'non-terminal 'index #t equation (cons bottom equivalence-function)))
      ((_ spec* att-name* ((non-terminal index) cached? equation bottom equivalence-function))
       (specify-attribute spec* att-name* 'non-terminal 'index cached? equation (cons bottom equivalence-function)))
      ((_ spec* att-name* (non-terminal equation))
       (specify-attribute spec* att-name* 'non-terminal 0 #t equation #f))
      ((_ spec* att-name* (non-terminal cached? equation))
       (specify-attribute spec* att-name* 'non-terminal 0 cached? equation #f))
      ((_ spec* att-name* (non-terminal equation bottom equivalence-function))
       (specify-attribute spec* att-name* 'non-terminal 0 #t equation (cons bottom equivalence-function)))
      ((_ spec* att-name* (non-terminal cached? equation bottom equivalence-function))
       (specify-attribute spec* att-name* 'non-terminal 0 cached? equation (cons bottom equivalence-function)))))
 
  (define specify-attribute
    (lambda (spec attribute-name non-terminal context-name-or-position cached? equation circularity-definition)
      ;;; Before adding the attribute definition, ensure...
      (let ((wrong-argument-type ; ...correct argument types,...
             (or
              (and (not (symbol? attribute-name))
                   "Attribute name : symbol")
              (and (not (symbol? non-terminal))
                   "AST rule : non-terminal")
              (and (not (symbol? context-name-or-position))
                   (or (not (integer? context-name-or-position)) (< context-name-or-position 0))
                   "Production position : index or context-name")
              (and (not (procedure? equation))
                   "Attribute equation : function")
              (and circularity-definition
                   (not (pair? circularity-definition))
                   (not (procedure? (cdr circularity-definition)))
                   "Circularity definition : #f or (bottom-value equivalence-function) pair"))))
        (when wrong-argument-type
          (throw-exception
           "Invalid attribute definition; "
           "Wrong argument type (" wrong-argument-type ").")))
      (unless (= (racr-specification-specification-phase spec) 2) ; ...that the RACR system is in the correct specification phase,...
        (throw-exception
         "Unexpected " attribute-name " attribute definition; "
         "Attributes can only be defined in the AG specification phase."))
      (let ((ast-rule (racr-specification-find-rule spec non-terminal)))
        (unless ast-rule ; ...the given AST rule is defined,...
          (throw-exception
           "Invalid attribute definition; "
           "The non-terminal " non-terminal " is not defined."))
        (let* ((context? ; ...the given context exists,...
                (if (symbol? context-name-or-position)
                    (if (eq? context-name-or-position '*)
                        (car (ast-rule-production ast-rule))
                        (ast-rule-find-child-context ast-rule context-name-or-position))
                    (if (>= context-name-or-position (length (ast-rule-production ast-rule)))
                        (throw-exception
                         "Invalid attribute definition; "
                         "There exists no " context-name-or-position "'th position in the context of " non-terminal ".")
                        (list-ref (ast-rule-production ast-rule) context-name-or-position)))))
          (unless context?
            (throw-exception
             "Invalid attribute definition; "
             "The non-terminal " non-terminal " has no " context-name-or-position " context."))
          (unless (symbol-non-terminal? context?) ; ...it is a non-terminal and...
            (throw-exception
             "Invalid attribute definition; "
             non-terminal context-name-or-position " is a terminal."))
          ; ...the attribute is not already defined for it:
          (when (memq attribute-name (map attribute-definition-name (symbol-attributes context?)))
            (throw-exception
             "Invalid attribute definition; "
             "Redefinition of " attribute-name " for " non-terminal context-name-or-position "."))
          ;;; Everything is fine. Thus, add the definition to the AST rule's respective symbol:
          (symbol-attributes-set!
           context?
           (cons
            (make-attribute-definition
             attribute-name
             context?
             equation
             circularity-definition
             cached?)
            (symbol-attributes context?)))))))
 
  (define compile-ag-specifications
    (lambda (spec)
      ;;; Ensure, that the RACR system is in the correct specification phase and...
      (let ((current-phase (racr-specification-specification-phase spec)))
        (when (< current-phase 2)
          (throw-exception
           "Unexpected AG compilation; "
           "The AST specifications are not yet compiled."))
        (if (> current-phase 2)
            (throw-exception
             "Unexpected AG compilation; "
             "The AG specifications already have been compiled.")
            (racr-specification-specification-phase-set! spec (+ current-phase 1)))) ; ...if so proceed to the next specification phase.
     
      ;;; Resolve attribute definitions inherited from a supertype. Thus,...
      (apply-wrt-ast-inheritance ; ...for every AST rule R which has a supertype...
       (lambda (rule)
         (let loop ((super-prod (ast-rule-production (ast-rule-supertype? rule)))
                    (sub-prod (ast-rule-production rule)))
           (unless (null? super-prod)
             (for-each ; ...check for every attribute definition of R's supertype...
              (lambda (super-att-def)
                (unless (find ; ...if it is shadowed by an attribute definition of R....
                         (lambda (sub-att-def)
                           (eq? (attribute-definition-name sub-att-def) (attribute-definition-name super-att-def)))
                         (symbol-attributes (car sub-prod)))
                  (symbol-attributes-set! ; ...If not, add...
                   (car sub-prod)
                   (cons
                    (make-attribute-definition ; ...a copy of the attribute definition inherited...
                     (attribute-definition-name super-att-def)
                     (car sub-prod) ; ...to R.
                     (attribute-definition-equation super-att-def)
                     (attribute-definition-circularity-definition super-att-def)
                     (attribute-definition-cached? super-att-def))
                    (symbol-attributes (car sub-prod))))))
              (symbol-attributes (car super-prod)))
             (loop (cdr super-prod) (cdr sub-prod)))))
       (racr-specification-rules-list spec))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Attribute Evaluation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; INTERNAL FUNCTION: Given a node n find a certain attribute associated with it, whereas in case no proper
  ; attribute is associated with n itself the search is extended to find a broadcast solution. If the
  ; extended search finds a solution, appropriate copy propergation attributes (i.e., broadcasters) are added.
  ; If no attribute instance can be found or n is a bud node, an exception is thrown. Otherwise, the
  ; attribute or its respective last broadcaster is returned.
  (define lookup-attribute
    (lambda (name n)
      (when (node-bud-node? n)
        (throw-exception
         "AG evaluator exception; "
         "Cannot access " name " attribute - the given node is a bud."))
      (let loop ((n n)) ; Recursively...
        (let ((att (node-find-attribute n name))) ; ...check if the current node has a proper attribute instance....
          (if att
              att ; ...If it has, return the found defining attribute instance.
              (let ((parent (node-parent n))) ; ...If no defining attribute instance can be found...
                (if (not parent) ; ...check if there exists a parent node that may provide a definition....
                    (throw-exception ; ...If not, throw an exception,...
                     "AG evaluator exception; "
                     "Cannot access unknown " name " attribute.")
                    (let* ((att (loop parent)) ; ...otherwise proceed the search at the parent node. If it succeeds...
                           (broadcaster ; ...construct a broadcasting attribute instance...
                            (make-attribute-instance
                             (make-attribute-definition ; ...whose definition context depends...
                              name
                              (if (eq? (node-ast-rule parent) 'list-node) ; ...if the parent node is a list node or not....
                                  (list-ref ; ...If it is a list node the broadcaster's context is...
                                   (ast-rule-production (node-ast-rule (node-parent parent))) ; ...the list node's parent node and...
                                   (node-child-index? parent)) ; ...child position.
                                  (list-ref ; ...If the parent node is not a list node the broadcaster's context is...
                                   (ast-rule-production (node-ast-rule parent)) ; ...the parent node and...
                                   (node-child-index? n))) ; ...the current node's child position. Further,...
                              (lambda (n . args) ; ...the broadcaster's equation just calls the parent node's counterpart. Finally,...
                                (apply att-value name (ast-parent n) args))
                              (attribute-definition-circularity-definition (attribute-instance-definition att))
                              #f)
                             n)))
                      (node-attributes-set! n (cons broadcaster (node-attributes n))) ; ...add the constructed broadcaster and...
                      broadcaster)))))))) ; ...return it as the current node's look-up result.
 
  (define att-value
    (lambda (name n . args)
      (let*-values (; The evaluator state used and changed throughout evaluation:
                    ((evaluator-state) (values (node-evaluator-state n)))
                    ; The attribute instance to evaluate:
                    ((att) (values (lookup-attribute name n)))
                    ; The attribute's definition:
                    ((att-def) (values (attribute-instance-definition att)))
                    ; The attribute cache entries used for evaluation and dependency tracking:
                    ((evaluation-att-cache dependency-att-cache)
                     (if (attribute-definition-cached? att-def)
                         ; If the attribute instance is cached, no special action is required, except...
                         (let ((att-cache
                                (or
                                 ; ...finding the attribute cache entry to use...
                                 (hashtable-ref (attribute-instance-cache att) args #f)
                                 ; ...or construct a respective one.
                                 (let ((new-entry (make-attribute-cache-entry att args)))
                                   (hashtable-set! (attribute-instance-cache att) args new-entry)
                                   new-entry))))
                           (values att-cache att-cache))
                         ; If the attribute is not cached, special attention must be paid to avoid the permament storing
                         ; of fixpoint results and attribute arguments on the one hand but still retaining correct
                         ; evaluation which requires these information on the other hand. To do so we introduce two
                         ; different types of attribute cache entries:
                         ; (1) A parameter approximating entry for tracking dependencies and influences of the uncached
                         ;     attribute instance.
                         ; (2) A set of temporary cycle entries for correct cycle detection and fixpoint computation.
                         ; The "cycle-value" field of the parameter approximating entry is misused to store the hashtable
                         ; containing the temporary cycle entries and must be deleted when evaluation finished.
                         (let* ((dependency-att-cache
                                 (or
                                  (hashtable-ref (attribute-instance-cache att) racr-nil #f)
                                  (let ((new-entry (make-attribute-cache-entry att racr-nil)))
                                    (hashtable-set! (attribute-instance-cache att) racr-nil new-entry)
                                    (attribute-cache-entry-cycle-value-set!
                                     new-entry
                                     (make-hashtable equal-hash equal? 1))
                                    new-entry)))
                                (evaluation-att-cache
                                 (or
                                  (hashtable-ref (attribute-cache-entry-cycle-value dependency-att-cache) args #f)
                                  (let ((new-entry (make-attribute-cache-entry att args)))
                                    (hashtable-set!
                                     (attribute-cache-entry-cycle-value dependency-att-cache)
                                     args
                                     new-entry)
                                    new-entry))))
                           (values evaluation-att-cache dependency-att-cache))))
                    ; Support function that given an intermediate fixpoint result checks if it is different from the
                    ; current cycle value and updates the cycle value and evaluator state accordingly:
                    ((update-cycle-cache)
                     (values
                      (lambda (new-result)
                        (unless ((cdr (attribute-definition-circularity-definition att-def))
                                 new-result
                                 (attribute-cache-entry-cycle-value evaluation-att-cache))
                          (attribute-cache-entry-cycle-value-set! evaluation-att-cache new-result)
                          (evaluator-state-ag-cycle-change?-set! evaluator-state #t))))))
        ; Decide how to evaluate the attribute dependening on whether its value already is cached or its respective
        ; cache entry is circular, already in evaluation or starting point of a fix-point computation:
        (cond
          ; CASE (0): Attribute already evaluated for given arguments:
          ((not (eq? (attribute-cache-entry-value evaluation-att-cache) racr-nil))
           ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
           ; evaluation of another entry, the other entry depends on this one. Afterwards,...
           (add-dependency:cache->cache dependency-att-cache)
           (attribute-cache-entry-value evaluation-att-cache)) ; ...return the cached value.
         
          ; CASE (1): Circular attribute that is starting point of a fixpoint computation:
          ((and (attribute-definition-circular? att-def) (not (evaluator-state-ag-in-cycle? evaluator-state)))
           (dynamic-wind
            (lambda ()
              ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
              ; evaluation of another entry, the other depends on this one. Further this entry depends
              ; on any other entry that will be evaluated through its own evaluation. Further,..
              (add-dependency:cache->cache dependency-att-cache)
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
              ; ...mark, that the entry is in evaluation and...
              (attribute-cache-entry-entered?-set! evaluation-att-cache #t)
              ; ...update the evaluator's state that we are about to start a fix-point computation.
              (evaluator-state-ag-in-cycle?-set! evaluator-state #t))
            (lambda ()
              (let loop () ; Start fix-point computation. Thus, as long as...
                (evaluator-state-ag-cycle-change?-set! evaluator-state #f) ; ...an entry's value changes...
                (update-cycle-cache (apply (attribute-definition-equation att-def) n args)) ; ...evaluate this entry.
                (when (evaluator-state-ag-cycle-change? evaluator-state)
                  (loop)))
              (let ((result (attribute-cache-entry-cycle-value evaluation-att-cache)))
                ; When fixpoint computation finished update the caches of all circular entries evaluated. To do so,...
                (let loop ((att-cache
                            (if (attribute-definition-cached? att-def)
                                evaluation-att-cache
                                dependency-att-cache)))
                  (let ((att-def (attribute-instance-definition (attribute-cache-entry-context att-cache))))
                    (if (not (attribute-definition-circular? att-def))
                        ; ...ignore non-circular entries and just proceed with the entries they depend on (to
                        ; ensure all strongly connected components within a weakly connected one are updated)....
                        (for-each
                         loop
                         (attribute-cache-entry-cache-dependencies att-cache))
                        ; ...In case of circular entries...
                        (if (attribute-definition-cached? att-def) ; ...check if they have to be cached and...
                            (when (eq? (attribute-cache-entry-value att-cache) racr-nil) ; ...are not already processed....
                              ; ...If so cache them,...
                              (attribute-cache-entry-value-set!
                               att-cache
                               (attribute-cache-entry-cycle-value att-cache))
                              (attribute-cache-entry-cycle-value-set! ; ...reset their cycle values to the bottom value and...
                               att-cache
                               (car (attribute-definition-circularity-definition att-def)))
                              (for-each ; ...proceed with the entries they depend on.
                               loop
                               (attribute-cache-entry-cache-dependencies att-cache)))
                            ; ...If a circular entry is not cached, check if it already is processed....
                            (when (> (hashtable-size (attribute-cache-entry-cycle-value att-cache)) 0)
                              ; ...If not, delete its temporary cycle cache and...
                              (hashtable-clear! (attribute-cache-entry-cycle-value att-cache))
                              (for-each ; ...proceed with the entries it depends on.
                               loop
                               (attribute-cache-entry-cache-dependencies att-cache)))))))
                result))
            (lambda ()
              ; Mark that fixpoint computation finished,...
              (evaluator-state-ag-in-cycle?-set! evaluator-state #f)
              ; the evaluation of the attribute cache entry finished and...
              (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
              ; ...pop the entry from the evaluation stack.
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cdr (evaluator-state-evaluation-stack evaluator-state))))))
         
          ; CASE (2): Circular attribute already in evaluation for the given arguments:
          ((and (attribute-definition-circular? att-def) (attribute-cache-entry-entered? evaluation-att-cache))
           ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
           ; evaluation of another entry, the other entry depends on this one. Finally,...
           (add-dependency:cache->cache dependency-att-cache)
           ; ...the intermediate fixpoint result is the attribute cache entry's cycle value.
           (attribute-cache-entry-cycle-value evaluation-att-cache))
         
          ; CASE (3): Circular attribute not in evaluation and entered throughout a fixpoint computation:
          ((attribute-definition-circular? att-def)
           (dynamic-wind
            (lambda ()
              ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
              ; evaluation of another entry, the other depends on this one. Further this entry depends
              ; on any other entry that will be evaluated through its own evaluation. Further,..
              (add-dependency:cache->cache dependency-att-cache)
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
              ; ...mark, that the entry is in evaluation.
              (attribute-cache-entry-entered?-set! evaluation-att-cache #t))
            (lambda ()
              (let ((result (apply (attribute-definition-equation att-def) n args))) ; Evaluate the entry and...
                (update-cycle-cache result) ; ...update its cycle value.
                result))
            (lambda ()
              ; Mark that the evaluation of the attribute cache entry finished and...
              (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
              ; ...pop it from the evaluation stack.
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cdr (evaluator-state-evaluation-stack evaluator-state))))))
         
          ; CASE (4): Non-circular attribute already in evaluation, i.e., unexpected cycle:
          ((attribute-cache-entry-entered? evaluation-att-cache)
           ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
           ; evaluation of another entry, the other entry depends on this one. Then,...
           (add-dependency:cache->cache dependency-att-cache)
           (throw-exception ; ...thrown an exception because we encountered an unexpected dependency cycle.
            "AG evaluator exception; "
            "Unexpected " name " cycle."))
         
          (else ; CASE (5): Non-circular attribute not in evaluation:
           (dynamic-wind
            (lambda ()
              ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
              ; evaluation of another entry, the other depends on this one. Further this entry depends
              ; on any other entry that will be evaluated through its own evaluation. Further,...
              (add-dependency:cache->cache dependency-att-cache)
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
              ; ...mark, that the entry is in evaluation.
              (attribute-cache-entry-entered?-set! evaluation-att-cache #t))
            (lambda ()
              (let ((result (apply (attribute-definition-equation att-def) n args))) ; Evaluate the entry and,...
                (when (attribute-definition-cached? att-def) ; ...if caching is enabled,...
                  (attribute-cache-entry-value-set! evaluation-att-cache result)) ; ...cache its value.
                result))
            (lambda ()
              ; Mark that the evaluation of the attribute cache entry finished and...
              (if (attribute-definition-cached? att-def)
                  (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
                  (hashtable-delete! (attribute-cache-entry-cycle-value dependency-att-cache) args))
              ; ...pop it from the evaluation stack.
              (evaluator-state-evaluation-stack-set!
               evaluator-state
               (cdr (evaluator-state-evaluation-stack evaluator-state))))))))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specification Queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; General Note: Because RACR specifications never change after compilation, there is no need to add and
  ;   maintain dependencies when attributes query specifications. The specification query API therefore just
  ;   forwards to the respective internal functions. Lists must be copied before they are returned however.
 
  ; Specification Queries:
 
  (define specification->phase
    (lambda (spec)
      (racr-specification-specification-phase spec)))
 
  (define specification->start-symbol
    (lambda (spec)
      (racr-specification-start-symbol spec)))
 
  (define specification->ast-rules
    (lambda (spec)
      (racr-specification-rules-list spec))) ; Already creates copy!
 
  (define specification->find-ast-rule
    (lambda (spec node-type)
      (racr-specification-find-rule spec node-type)))
 
  ; AST Rule Queries:
 
  (define ast-rule->symbolic-representation
    (lambda (ast-rule)
      (ast-rule-as-symbol ast-rule)))
 
  (define ast-rule->supertype?
    (lambda (ast-rule)
      (ast-rule-supertype? ast-rule)))
 
  (define ast-rule->production
    (lambda (rule)
      (append (ast-rule-production rule) (list)))) ; Create copy!
 
  ; Production Symbol Queries:
 
  (define symbol->name
    (lambda (symb)
      (symbol-name symb)))
 
  (define symbol->non-terminal?
    (lambda (symb)
      (symbol-non-terminal? symb)))
 
  (define symbol->kleene?
    (lambda (symb)
      (symbol-kleene? symb)))
 
  (define symbol->context-name
    (lambda (symb)
      (symbol-context-name symb)))
 
  (define symbol->attributes
    (lambda (symbol)
      (append (symbol-attributes symbol) (list)))) ; Create copy!
 
  ; Attribute Definition Queries:
 
  (define attribute->name
    (lambda (att-def)
      (attribute-definition-name att-def)))
 
  (define attribute->circular?
    (lambda (att-def)
      (attribute-definition-circular? att-def)))
 
  (define attribute->synthesized?
    (lambda (att-def)
      (attribute-definition-synthesized? att-def)))
 
  (define attribute->inherited?
    (lambda (att-def)
      (attribute-definition-inherited? att-def)))
 
  (define attribute->cached?
    (lambda (att-def)
      (attribute-definition-cached? att-def)))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (define ast-node? ; Scheme entities are either allocated as AST nodes or never will be => No need to add dependencies!
    (lambda (n)
      (node? n)))
 
  (define ast-specification
    (lambda (n)
      (when (or (ast-list-node? n) (ast-bud-node? n)) ; Remember: Terminal nodes as such are never exposed to users.
        (throw-exception
         "Cannot query specification; "
         "List and bud nodes are not part of any specification."))
      ; The specification of a node can never change => No need to add dependencies!
      (ast-rule-specification (node-ast-rule n))))
 
  (define ast-list-node? ; No dependency tracking needed!
    (lambda (n)
      (node-list-node? n)))
 
  (define ast-bud-node? ; No dependency tracking needed!
    (lambda (n)
      (node-bud-node? n)))
 
  (define ast-node-rule
    (lambda (n)
      (when (or (ast-list-node? n) (ast-bud-node? n)) ; Remember: Terminal nodes as such are never exposed to users.
        (throw-exception
         "Cannot query type; "
         "List and bud nodes have no type."))
      (add-dependency:cache->node-type n)
      (node-ast-rule n)))
 
  (define ast-node-type
    (lambda (n)
      (symbol-name (car (ast-rule-production (ast-node-rule n))))))
 
  (define ast-subtype?
    (lambda (a1 a2)
      (when (or
             (and (ast-node? a1) (or (ast-list-node? a1) (ast-bud-node? a1)))
             (and (ast-node? a2) (or (ast-list-node? a2) (ast-bud-node? a2))))
        (throw-exception
         "Cannot perform subtype check; "
         "List and bud nodes cannot be tested for subtyping."))
      (when (and (not (ast-node? a1)) (not (ast-node? a2)))
        (throw-exception
         "Cannot perform subtype check; "
         "At least one argument must be an AST node."))
      ((lambda (t1/t2)
         (and
          (car t1/t2)
          (cdr t1/t2)
          (ast-rule-subtype? (car t1/t2) (cdr t1/t2))))
       (if (symbol? a1)
           (let* ((t2 (node-ast-rule a2))
                  (t1 (racr-specification-find-rule (ast-rule-specification t2) a1)))
             (unless t1
               (throw-exception
                "Cannot perform subtype check; "
                a1 " is no valid non-terminal (first argument undefined non-terminal)."))
             (add-dependency:cache->node-super-type a2 t1)
             (cons t1 t2))
           (if (symbol? a2)
               (let* ((t1 (node-ast-rule a1))
                      (t2 (racr-specification-find-rule (ast-rule-specification t1) a2)))
                 (unless t1
                   (throw-exception
                    "Cannot perform subtype check; "
                    a2 " is no valid non-terminal (second argument undefined non-terminal)."))
                 (add-dependency:cache->node-sub-type a1 t2)
                 (cons t1 t2))
               (begin
                 (add-dependency:cache->node-sub-type a1 (node-ast-rule a2))
                 (add-dependency:cache->node-super-type a2 (node-ast-rule a1))
                 (cons (node-ast-rule a1) (node-ast-rule a2))))))))
 
  (define ast-has-parent?
    (lambda (n)
      (let ((parent (node-parent n)))
        (if parent
            (begin
              (add-dependency:cache->node parent)
              parent)
            (begin
              (add-dependency:cache->node-is-root n)
              #f)))))
 
  (define ast-parent
    (lambda (n)
      (let ((parent (node-parent n)))
        (unless parent
          (throw-exception "Cannot query parent of roots."))
        (add-dependency:cache->node parent)
        parent)))
 
  (define ast-has-child?
    (lambda (context-name n)
      (add-dependency:cache->node-defines-context n context-name)
      (if (node-find-child n context-name) #t #f))) ; BEWARE: Never return the child if it exists, but instead just #t!
 
  (define ast-child
    (lambda (i n)
      (let ((child
             (if (symbol? i)
                 (node-find-child n i)
                 (and (>= i 1) (<= i (length (node-children n))) (list-ref (node-children n) (- i 1))))))
        (unless child
          (throw-exception "Cannot query non-existent " i (if (symbol? i) "" "'th") " child."))
        (add-dependency:cache->node child)
        (if (node-terminal? child)
            (node-children child)
            child))))
 
  (define ast-has-sibling?
    (lambda (context-name n)
      (let ((parent? (ast-has-parent? n)))
        (and parent? (ast-has-child? context-name parent?)))))
 
  (define ast-sibling
    (lambda (i n)
      (ast-child i (ast-parent n))))
 
  (define ast-child-index
    (lambda (n)
      (ast-find-child*
       (lambda (i child)
         (if (eq? child n) i #f))
       (ast-parent n))))
 
  (define ast-num-children
    (lambda (n)
      (add-dependency:cache->node-num-children n)
      (length (node-children n))))
 
  (define ast-children
    (lambda (n . b)
      (reverse
       (let ((result (list)))
         (apply
          ast-for-each-child
          (lambda (i child)
            (set! result (cons child result)))
          n
          b)
         result))))
 
  (define ast-for-each-child
    (lambda (f n . b)
      (let ((b (if (null? b) (list (cons 1 '*)) b)))
        (for-each
         (lambda (b)
           (if (eq? (cdr b) '*)
               (let ((pos (car b))
                     (ub (length (node-children n))))
                 (dynamic-wind
                  (lambda () #f)
                  (lambda ()
                    (let loop ()
                      (when (<= pos ub)
                        (f pos (ast-child pos n))
                        (set! pos (+ pos 1))
                        (loop))))
                  (lambda ()
                    (when (> pos ub)
                      (ast-num-children n))))) ; BEWARE: Access to number of children ensures proper dependency tracking!
               (let loop ((pos (car b)))
                 (when (<= pos (cdr b))
                   (f pos (ast-child pos n))
                   (loop (+ pos 1))))))
         b))))
 
  (define ast-find-child
    (lambda (f n . b)
      (call/cc
       (lambda (c)
         (apply
          ast-for-each-child
          (lambda (i child)
            (when (f i child)
              (c child)))
          n
          b)
         #f))))
 
  (define ast-find-child*
    (lambda (f n . b)
      (call/cc
       (lambda (c)
         (apply
          ast-for-each-child
          (lambda (i child)
            (let ((res (f i child)))
              (when res
                (c res))))
          n
          b)
         #f))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Construction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  (define create-ast
    (lambda (spec rule children)
      ;;; Before constructing the node ensure, that...
      (when (< (racr-specification-specification-phase spec) 3) ; ...the RACR system is completely specified,...
        (throw-exception
         "Cannot construct " rule " fragment; "
         "The RACR specification still must be compiled."))
      (let* ((ast-rule (racr-specification-find-rule spec rule))
             (new-fragment
              (make-node
               ast-rule
               #f
               (list))))
        (unless ast-rule ; ...the given AST rule is defined,...
          (throw-exception
           "Cannot construct " rule " fragment; "
           "Unknown non-terminal/rule."))
        (unless (satisfies-contexts? children (cdr (ast-rule-production ast-rule))) ; ...and the children fit.
          (throw-exception
           "Cannot construct " rule " fragment; "
           "The given children do not fit."))
        ;;; When all constraints are satisfied, construct the new fragment,...
        (node-children-set! ; ...add its children,...
         new-fragment
         (map ; ...set it as parent of each child,...
          (lambda (symbol child)
            (if (symbol-non-terminal? symbol)
                (begin
                  (for-each ; ...flush all attribute cache entries depending on any added child being a root,...
                   (lambda (influence)
                     (flush-attribute-cache-entry (car influence)))
                   (filter
                    (lambda (influence)
                      (vector-ref (cdr influence) 1))
                    (node-cache-influences child)))
                  (node-parent-set! child new-fragment)
                  child)
                (make-node 'terminal new-fragment child)))
          (cdr (ast-rule-production ast-rule))
          children))
        (distribute-evaluator-state (make-evaluator-state) new-fragment) ; ...distribute the new fragment's evaluator state and...
        (update-synthesized-attribution new-fragment) ; ...initialize its synthesized and...
        (for-each ; ...each child's inherited attributes.
         update-inherited-attribution
         (node-children new-fragment))
        new-fragment))) ; Finally, return the newly constructed fragment.
 
  (define create-ast-list
    (lambda (children)
      ;;; Before constructing the list node ensure, that...
      (let ((new-list
             (make-node
              'list-node
              #f
              (append children (list))))) ; BEWARE: create copy of children!
        (unless
            (for-all ; ...all children fit.
                (lambda (child)
                  (valid-list-element-candidate? new-list child))
              children)
          (throw-exception
           "Cannot construct list node; "
           "The given children do not fit."))
        ;;; When all constraints are satisfied,...
        (for-each ; ...flush all attribute cache entries depending on the children being roots,...
         (lambda (child)
           (for-each
            (lambda (influence)
              (flush-attribute-cache-entry (car influence)))
            (filter
             (lambda (influence)
               (vector-ref (cdr influence) 1))
             (node-cache-influences child))))
         children)
        (for-each ; ...set the new list node as parent of every child,...
         (lambda (child)
           (node-parent-set! child new-list))
         children)
        (distribute-evaluator-state (make-evaluator-state) new-list) ; ...construct and distribute its evaluator state and...
        new-list))) ; ...return it.
 
  (define create-ast-bud
    (lambda ()
      (let ((bud-node (make-node 'bud-node #f (list))))
        (distribute-evaluator-state (make-evaluator-state) bud-node)
        bud-node)))
 
  (define create-ast-mockup
    (lambda (rule)
      (create-ast
       (ast-rule-specification rule)
       (symbol-name (car (ast-rule-production rule)))
       (map
        (lambda (symbol)
          (cond
            ((not (symbol-non-terminal? symbol))
             racr-nil)
            ((symbol-kleene? symbol)
             (create-ast-list (list)))
            (else (create-ast-bud))))
        (cdr (ast-rule-production rule))))))
 
  ; INTERNAL FUNCTION: Given two non-terminal nodes, return if the second can replace the first regarding its context.
  (define valid-replacement-candidate?
    (lambda (node candidate)
      (if (node-list-node? (node-parent node))
          (valid-list-element-candidate? (node-parent node) candidate)
          (and
           (satisfies-context?
            candidate
            (list-ref (ast-rule-production (node-ast-rule (node-parent node))) (node-child-index? node)))
           (not (node-inside-of? node candidate))))))
 
  ; INTERNAL FUNCTION: Given a list node and another node, return if the other node can become element of
  ; the list node regarding its context.
  (define valid-list-element-candidate?
    (lambda (list-node candidate)
      (let ((expected-type? ; If the list node has a parent, its parent induces a type for the list's elements.
             (if (node-parent list-node)
                 (symbol-non-terminal?
                  (list-ref
                   (ast-rule-production (node-ast-rule (node-parent list-node)))
                   (node-child-index? list-node)))
                 #f)))
        (and ; The given candidate can be element of the list, if (1)...
         (if expected-type? ; ...either,...
             (satisfies-context? candidate expected-type? #f) ; ...the candidate fits regarding the context in which the list is, or,...
             (and ; ...in case no type is induced for the list's elements,...
              (ast-node? candidate) ; ...the candiate is a non-terminal node,...
              (not (node-list-node? candidate)) ; ...not a list node,...
              (not (node-parent candidate)) ; ...not already part of another AST and...
              (not (evaluator-state-in-evaluation? (node-evaluator-state candidate))))) ; ...non of its attributes are in evaluation,...
         (not (node-inside-of? list-node candidate)))))) ; ...and (2) its spaned AST does not contain the list node.
 
  ; INTERNAL FUNCTION: Given a node or terminal value and a context, return if the
  ; node/terminal value can become a child of the given context.
  (define satisfies-context?
    (case-lambda
      ((child context)
       (satisfies-context? child (symbol-non-terminal? context) (symbol-kleene? context)))
      ((child non-terminal? kleene?)
       (or ; The given child is valid if either,...
        (not non-terminal?) ; ...a terminal is expected or,...
        (and ; ...in case a non-terminal is expected,...
         (ast-node? child) ; ...the given child is an AST node,...
         (not (node-parent child)) ; ...does not already belong to another AST,...
         (not (evaluator-state-in-evaluation? (node-evaluator-state child))) ; ...non of its attributes are in evaluation and...
         (or
          (node-bud-node? child) ; ...the child either is a bud node or,...
          (if kleene?
              (and ; ...in case a list node is expected,...
               (node-list-node? child) ; ...is a list...
               (for-all ; ...whose children are...
                   (lambda (child)
                     (or ; ...either bud nodes or nodes of the expected type, or,...
                      (node-bud-node? child)
                      (ast-rule-subtype? (node-ast-rule child) non-terminal?)))
                 (node-children child)))
              (and ; ...in case a non-list node is expected,...
               (not (node-list-node? child)) ; ...is a non-list node of...
               (ast-rule-subtype? (node-ast-rule child) non-terminal?))))))))) ; ...the expected type.
 
  ; INTERNAL FUNCTION: Given list of nodes or terminal values and a list of contexts, return if the
  ; nodes/terminal values can become children of the given contexts.
  (define satisfies-contexts?
    (lambda (children contexts)
      (and
       (= (length children) (length contexts))
       (for-all satisfies-context? children contexts))))
 
  ; INTERNAL FUNCTION: Given an AST node update its synthesized attribution (i.e., add missing synthesized
  ; attributes, delete superfluous ones, shadow equally named inherited attributes and update the
  ; definitions of existing synthesized attributes.
  (define update-synthesized-attribution
    (lambda (n)
      (when (and (not (node-terminal? n)) (not (node-list-node? n)) (not (node-bud-node? n)))
        (for-each
         (lambda (att-def)
           (let ((att (node-find-attribute n (attribute-definition-name att-def))))
             (cond
               ((not att)
                (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n))))
               ((eq? (attribute-definition-equation (attribute-instance-definition att)) (attribute-definition-equation att-def))
                (attribute-instance-definition-set! att att-def))
               (else
                (flush-attribute-instance att)
                (node-attributes-set!
                 n
                 (cons (make-attribute-instance att-def n) (remq att (node-attributes n))))))))
         (symbol-attributes (car (ast-rule-production (node-ast-rule n)))))
        (node-attributes-set! ; Delete all synthesized attribute instances not defined anymore:
         n
         (remp
          (lambda (att)
            (let ((remove?
                   (and
                    (attribute-definition-synthesized? (attribute-instance-definition att))
                    (not
                     (eq?
                      (symbol-ast-rule (attribute-definition-context (attribute-instance-definition att)))
                      (node-ast-rule n))))))
              (when remove?
                (flush-attribute-instance att))
              remove?))
          (node-attributes n))))))
 
  ; INTERNAL FUNCTION: Given an AST node update its inherited attribution (i.e., add missing inherited
  ; attributes, delete superfluous ones and update the definitions of existing inherited attributes.
  ; If the given node is a list-node the inherited attributes of its elements are updated.
  (define update-inherited-attribution
    (lambda (n)
      ;;; Support function updating n's inherited attribution w.r.t. a list of inherited attribute definitions:
      (define update-by-defs
        (lambda (n att-defs)
          (for-each ; Add new and update existing inherited attribute instances:
           (lambda (att-def)
             (let ((att (node-find-attribute n (attribute-definition-name att-def))))
               (cond
                 ((not att)
                  (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n))))
                 ((not (attribute-definition-synthesized? (attribute-instance-definition att)))
                  (if (eq?
                       (attribute-definition-equation (attribute-instance-definition att))
                       (attribute-definition-equation att-def))
                      (attribute-instance-definition-set! att att-def)
                      (begin
                        (flush-attribute-instance att)
                        (node-attributes-set!
                         n
                         (cons (make-attribute-instance att-def n) (remq att (node-attributes n))))))))))
           att-defs)
          (node-attributes-set! ; Delete all inherited attribute instances not defined anymore:
           n
           (remp
            (lambda (att)
              (let ((remove?
                     (and
                      (attribute-definition-inherited? (attribute-instance-definition att))
                      (not (memq (attribute-instance-definition att) att-defs)))))
                (when remove?
                  (flush-attribute-instance att))
                remove?))
            (node-attributes n)))))
      ;;; Perform the update:
      (let* ((parent (node-parent n))
             (att-defs
              (cond
                ((not parent)
                 (list))
                ((not (node-list-node? parent))
                 (symbol-attributes
                  (list-ref
                   (ast-rule-production (node-ast-rule parent))
                   (node-child-index? n))))
                ((node-parent parent)
                 (symbol-attributes
                  (list-ref
                   (ast-rule-production (node-ast-rule (node-parent parent)))
                   (node-child-index? parent))))
                (else (list)))))
        (if (node-list-node? n)
            (for-each
             (lambda (n)
               (unless (node-bud-node? n)
                 (update-by-defs n att-defs)))
             (node-children n))
            (unless (node-bud-node? n)
              (update-by-defs n att-defs))))))
 
  ; INTERNAL FUNCTION: Given an AST node delete its inherited attribute instances. Iff the given node
  ; is a list node, the inherited attributes of its elements are deleted.
  (define detach-inherited-attributes
    (lambda (n)
      (cond
        ((node-list-node? n)
         (for-each
          detach-inherited-attributes
          (node-children n)))
        ((node-non-terminal? n)
         (node-attributes-set!
          n
          (remp
           (lambda (att)
             (let ((remove? (attribute-definition-inherited? (attribute-instance-definition att))))
               (when remove?
                 (flush-attribute-instance att))
               remove?))
           (node-attributes n)))))))
 
  ; INTERNAL FUNCTION: Given an evaluator state and an AST fragment, change the
  ; fragment's evaluator state to the given one.
  (define distribute-evaluator-state
    (lambda (evaluator-state n)
      (node-evaluator-state-set! n evaluator-state)
      (unless (node-terminal? n)
        (for-each
         (lambda (n)
           (distribute-evaluator-state evaluator-state n))
         (node-children n)))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency Tracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node
    (lambda (influencing-node)
      (add-dependency:cache->node-characteristic influencing-node (cons 0 racr-nil))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-is-root
    (lambda (influencing-node)
      (add-dependency:cache->node-characteristic influencing-node (cons 1 racr-nil))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-num-children
    (lambda (influencing-node)
      (add-dependency:cache->node-characteristic influencing-node (cons 2 racr-nil))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-type
    (lambda (influencing-node)
      (add-dependency:cache->node-characteristic influencing-node (cons 3 racr-nil))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-super-type
    (lambda (influencing-node comparision-type)
      (add-dependency:cache->node-characteristic influencing-node (cons 4 comparision-type))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-sub-type
    (lambda (influencing-node comparision-type)
      (add-dependency:cache->node-characteristic influencing-node (cons 5 comparision-type))))
 
  ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
  (define add-dependency:cache->node-defines-context
    (lambda (influencing-node context-name)
      (add-dependency:cache->node-characteristic influencing-node (cons 6 context-name))))
 
  ; INTERNAL FUNCTION: Given a node N and a correlation C add an dependency-edge marked with C from
  ; the attribute cache entry currently in evaluation (considering the evaluator state of the AST N
  ; is part of) to N and an influence-edge vice versa. If no attribute cache entry is in evaluation
  ; no edges are added. The following seven correlations exist:
  ;  0) Dependency on the existence of the node (i.e., existence of a node at the same location)
  ;  1) Dependency on the node being a root (i.e., the node has no parent)
  ;  2) Dependency on the node's number of children (i.e., existence of a node at the same location and with
  ;     the same number of children)
  ;  3) Dependency on the node's type (i.e., existence of a node at the same location and with the same type)
  ;  4) Dependency on whether the node's type is a supertype w.r.t. a certain type encoded in C or not
  ;  5) Dependency on whether the node's type is a subtype w.r.t. a certain type encoded in C or not
  ;  6) Dependency on whether the node defines a certain context (i.e., has child with a certain name) or not
  (define add-dependency:cache->node-characteristic
    (lambda (influencing-node correlation)
      (let ((dependent-cache (evaluator-state-in-evaluation? (node-evaluator-state influencing-node))))
        (when dependent-cache
          (let ((dependency-vector
                 (let ((dc-hit (assq influencing-node (attribute-cache-entry-node-dependencies dependent-cache))))
                   (and dc-hit (cdr dc-hit)))))
            (unless dependency-vector
              (set! dependency-vector (vector #f #f #f #f (list) (list) (list)))
              (attribute-cache-entry-node-dependencies-set!
               dependent-cache
               (cons
                (cons influencing-node dependency-vector)
                (attribute-cache-entry-node-dependencies dependent-cache)))
              (node-cache-influences-set!
               influencing-node
               (cons
                (cons dependent-cache dependency-vector)
                (node-cache-influences influencing-node))))
            (let ((correlation-type (car correlation))
                  (correlation-arg (cdr correlation)))
              (vector-set!
               dependency-vector
               correlation-type
               (case correlation-type
                 ((0 1 2 3)
                  #t)
                 ((4 5 6)
                  (let ((known-args (vector-ref dependency-vector correlation-type)))
                    (if (memq correlation-arg known-args)
                        known-args
                        (cons correlation-arg known-args))))))))))))
 
  ; INTERNAL FUNCTION: Given an attribute cache entry C, add an dependency-edge from C to the entry currently
  ; in evaluation (considering the evaluator state of the AST C is part of) and an influence-edge vice-versa.
  ; If no attribute cache entry is in evaluation no edges are added.
  (define add-dependency:cache->cache
    (lambda (influencing-cache)
      (let ((dependent-cache
             (evaluator-state-in-evaluation?
              (node-evaluator-state
               (attribute-instance-context
                (attribute-cache-entry-context influencing-cache))))))
        (when (and dependent-cache (not (memq influencing-cache (attribute-cache-entry-cache-dependencies dependent-cache))))
          (attribute-cache-entry-cache-dependencies-set!
           dependent-cache
           (cons
            influencing-cache
            (attribute-cache-entry-cache-dependencies dependent-cache)))
          (attribute-cache-entry-cache-influences-set!
           influencing-cache
           (cons
            dependent-cache
            (attribute-cache-entry-cache-influences influencing-cache)))))))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Rewriting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; INTERNAL FUNCTION: Given an attribute instance, flush all its cache entries.
  (define flush-attribute-instance
    (lambda (att)
      (call-with-values
       (lambda ()
         (hashtable-entries (attribute-instance-cache att)))
       (lambda (keys values)
         (vector-for-each
          flush-attribute-cache-entry
          values)))))
 
  ; INTERNAL FUNCTION: Given an attribute cache entry, delete it and all depending entries.
  (define flush-attribute-cache-entry
    (lambda (att-cache)
      (let ((influenced-caches (attribute-cache-entry-cache-influences att-cache))) ; Save all influenced attribute cache entries.
        ; Delete foreign influences:
        (for-each ; For every cache entry I the entry depends on,...
         (lambda (influencing-cache)
           (attribute-cache-entry-cache-influences-set! ; ...remove the influence edge from I to the entry.
            influencing-cache
            (remq att-cache (attribute-cache-entry-cache-influences influencing-cache))))
         (attribute-cache-entry-cache-dependencies att-cache))
        (for-each ; For every node N the attribute cache entry depends on...
         (lambda (node-dependency)
           (node-cache-influences-set!
            (car node-dependency)
            (remp ; ...remove the influence edge from N to the entry.
             (lambda (cache-influence)
               (eq? (car cache-influence) att-cache))
             (node-cache-influences (car node-dependency)))))
         (attribute-cache-entry-node-dependencies att-cache))
        ; Delete the attribute cache entry:
        (hashtable-delete!
         (attribute-instance-cache (attribute-cache-entry-context att-cache))
         (attribute-cache-entry-arguments att-cache))
        (attribute-cache-entry-cache-dependencies-set! att-cache (list))
        (attribute-cache-entry-node-dependencies-set! att-cache (list))
        (attribute-cache-entry-cache-influences-set! att-cache (list))
        ; Proceed flushing, i.e., for every attribute cache entry D the entry originally influenced,...
        (for-each
         (lambda (dependent-cache)
           (flush-attribute-cache-entry dependent-cache)) ; ...flush D.
         influenced-caches))))
 
  ; INTERNAL FUNCTION: Given an AST node n, flush all attribute cache entries that depend on
  ; information of the subtree spaned by n but are outside of it and, if requested, all attribute
  ; cache entries within the subtree spaned by n that depend on information outside of it.
  (define flush-inter-fragment-dependent-attribute-cache-entries
    (lambda (n flush-outgoing?)
      (let loop ((n* n))
        (for-each
         (lambda (influence)
           (unless (node-inside-of? (attribute-instance-context (attribute-cache-entry-context (car influence))) n)
             (flush-attribute-cache-entry (car influence))))
         (node-cache-influences n*))
        (for-each
         (lambda (att)
           (vector-for-each
            (lambda (att-cache)
              (let ((flush-att-cache?
                     (and
                      flush-outgoing?
                      (or
                       (find
                        (lambda (dependency)
                          (not (node-inside-of? (car dependency) n)))
                        (attribute-cache-entry-node-dependencies att-cache))
                       (find
                        (lambda (influencing-cache)
                          (not (node-inside-of? (attribute-instance-context (attribute-cache-entry-context influencing-cache)) n)))
                        (attribute-cache-entry-cache-dependencies att-cache))))))
                (if flush-att-cache?
                    (flush-attribute-cache-entry att-cache)
                    (for-each
                     (lambda (dependent-cache)
                       (unless (node-inside-of? (attribute-instance-context (attribute-cache-entry-context dependent-cache)) n)
                         (flush-attribute-cache-entry dependent-cache)))
                     (attribute-cache-entry-cache-influences att-cache)))))
            (call-with-values
             (lambda ()
               (hashtable-entries (attribute-instance-cache att)))
             (lambda (key-vector value-vector)
               value-vector))))
         (node-attributes n*))
        (unless (node-terminal? n*)
          (for-each
           loop
           (node-children n*))))))
 
  (define rewrite-terminal
    (lambda (i n new-value)
      ;;; Before changing the value of the terminal ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation and...
        (throw-exception
         "Cannot change terminal value; "
         "There are attributes in evaluation."))
      (let ((n
             (if (symbol? i)
                 (node-find-child n i)
                 (and (>= i 1) (<= i (length (node-children n))) (list-ref (node-children n) (- i 1))))))
        (unless (and n (node-terminal? n)) ; ...the given context is a terminal.
          (throw-exception
           "Cannot change terminal value; "
           "The given context does not exist or is no terminal."))
        ;;; Everything is fine. Thus,...
        (let ((old-value (node-children n)))
          (for-each ; ...flush all attribute cache entries influenced by the terminal,...
           (lambda (influence)
             (flush-attribute-cache-entry (car influence)))
           (node-cache-influences n))
          (node-children-set! n new-value) ; ...rewrite its value and...
          old-value)))) ; ...return its old value.
 
  (define rewrite-refine
    (lambda (n t . c)
      ;;; Before refining the non-terminal node ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...non of its attributes are in evaluation,...
        (throw-exception
         "Cannot refine node; "
         "There are attributes in evaluation."))
      (when (or (node-list-node? n) (node-bud-node? n)) ; ...it is not a list or bud node,...
        (throw-exception
         "Cannot refine node; "
         "The node is a " (if (node-list-node? n) "list" "bud") " node."))
      (let* ((old-rule (node-ast-rule n))
             (new-rule (racr-specification-find-rule (ast-rule-specification old-rule) t)))
        (unless (and new-rule (ast-rule-subtype? new-rule old-rule)) ; ...the given type is a subtype and...
          (throw-exception
           "Cannot refine node; "
           t " is not a subtype of " (symbol-name (car (ast-rule-production old-rule))) "."))
        (let ((additional-children (list-tail (ast-rule-production new-rule) (length (ast-rule-production old-rule)))))
          (unless (satisfies-contexts? c additional-children) ; ...all additional children fit.
            (throw-exception
             "Cannot refine node; "
             "The given additional children do not fit."))
          ;;; Everything is fine. Thus,...
          (for-each ; ...flush the influenced attribute cache entries, i.e., all entries influenced by the node's...
           (lambda (influence)
             (flush-attribute-cache-entry (car influence)))
           (filter
            (lambda (influence)
              (or
               (and (vector-ref (cdr influence) 2) (not (null? c))) ; ...number of children,...
               (and (vector-ref (cdr influence) 3) (not (eq? old-rule new-rule))) ; ...type,...
               (find ; ...supertype,...
                (lambda (t2)
                  (not (eq? (ast-rule-subtype? t2 old-rule) (ast-rule-subtype? t2 new-rule))))
                (vector-ref (cdr influence) 4))
               (find ; ...subtype or...
                (lambda (t2)
                  (not (eq? (ast-rule-subtype? old-rule t2) (ast-rule-subtype? new-rule t2))))
                (vector-ref (cdr influence) 5))
               (find ; ...defined contexts and...
                (lambda (context-name)
                  (let ((old-defines-context? (ast-rule-find-child-context old-rule context-name))
                        (new-defines-context? (ast-rule-find-child-context new-rule context-name)))
                    (if old-defines-context? (not new-defines-context?) new-defines-context?)))
                (vector-ref (cdr influence) 6))))
            (node-cache-influences n)))
          (for-each ; ...all entries depending on the new children being roots. Afterwards,...
           (lambda (child context)
             (when (symbol-non-terminal? context)
               (for-each
                (lambda (influence)
                  (flush-attribute-cache-entry (car influence)))
                (filter
                 (lambda (influence)
                   (vector-ref (cdr influence) 1))
                 (node-cache-influences child)))))
           c
           additional-children)
          (node-ast-rule-set! n new-rule) ; ...update the node's type,...
          (update-synthesized-attribution n) ; ...synthesized attribution,...
          (node-children-set! ; ...insert the new children and...
           n
           (append
            (node-children n)
            (map
             (lambda (child context)
               (let ((child
                      (if (symbol-non-terminal? context)
                          child
                          (make-node 'terminal n child))))
                 (node-parent-set! child n)
                 (distribute-evaluator-state (node-evaluator-state n) child) ; ...update their evaluator state and...
                 child))
             c
             additional-children)))
          (for-each
           update-inherited-attribution ; ...inherited attribution.
           (node-children n))))))
 
  (define rewrite-abstract
    (lambda (n t)
      ;;; Before abstracting the node ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation,...
        (throw-exception
         "Cannot abstract node; "
         "There are attributes in evaluation."))
      (when (or (node-list-node? n) (node-bud-node? n)) ; ...the node is not a list or bud node,...
        (throw-exception
         "Cannot abstract node; "
         "The node is a " (if (node-list-node? n) "list" "bud") " node."))
      (let* ((old-rule (node-ast-rule n))
             (new-rule (racr-specification-find-rule (ast-rule-specification old-rule) t)))
        (unless (and new-rule (ast-rule-subtype? old-rule new-rule)) ; ...the new type is a supertype and...
          (throw-exception
           "Cannot abstract node; "
           t " is not a supertype of " (symbol-name (car (ast-rule-production old-rule))) "."))
        ; ...permitted in the context in which the node is:
        (unless (or (not (node-parent n)) (valid-replacement-candidate? n (create-ast-mockup new-rule)))
          (throw-exception
           "Cannot abstract node; "
           "Abstraction to type " t " not permitted by context."))
        ;;; Everything is fine. Thus,...
        (let* ((num-new-children (length (cdr (ast-rule-production new-rule))))
               (children-to-remove (list-tail (node-children n) num-new-children)))
          (for-each ; ...flush all influenced attribute cache entries, i.e., all entries influenced by the node's...
           (lambda (influence)
             (flush-attribute-cache-entry (car influence)))
           (filter
            (lambda (influence)
              (or
               (and (vector-ref (cdr influence) 2) (not (null? children-to-remove))) ; ...number of children,...
               (and (vector-ref (cdr influence) 3) (not (eq? old-rule new-rule))) ; ...type...
               (find ; ...supertype,...
                (lambda (t2)
                  (not (eq? (ast-rule-subtype? t2 old-rule) (ast-rule-subtype? t2 new-rule))))
                (vector-ref (cdr influence) 4))
               (find ; ...subtype or...
                (lambda (t2)
                  (not (eq? (ast-rule-subtype? old-rule t2) (ast-rule-subtype? new-rule t2))))
                (vector-ref (cdr influence) 5))
               (find ; ...defined contexts and...
                (lambda (context-name)
                  (let ((old-defines-context? (ast-rule-find-child-context old-rule context-name))
                        (new-defines-context? (ast-rule-find-child-context new-rule context-name)))
                    (if old-defines-context? (not new-defines-context?) new-defines-context?)))
                (vector-ref (cdr influence) 6))))
            (node-cache-influences n)))
          (for-each ; ...all entries cross-depending the removed ASTs. Afterwards,...
           (lambda (child-to-remove)
             (flush-inter-fragment-dependent-attribute-cache-entries child-to-remove #t))
           children-to-remove)
          (node-ast-rule-set! n new-rule) ; ...update the node's type and its...
          (update-synthesized-attribution n) ; ...synthesized (because of possibly less) and...
          (update-inherited-attribution n) ; ...inherited (because of unshadowed) attributes. Further,...
          (for-each ; ...for every child to remove,...
           (lambda (child)
             (detach-inherited-attributes child) ; ...delete its inherited attributes,...
             (node-parent-set! child #f) ; ...detach it from the AST and...
             (distribute-evaluator-state (make-evaluator-state) child)) ; ...update its evaluator state. Then,...
           children-to-remove)
          (unless (null? children-to-remove)
            (if (> num-new-children 0)
                (set-cdr! (list-tail (node-children n) (- num-new-children 1)) (list))
                (node-children-set! n (list))))
          (for-each ; ...update the inherited attribution of all remaining children. Finally,...
           update-inherited-attribution
           (node-children n))
          (map ; ...return the removed children.
           (lambda (child) (if (node-terminal? child) (node-children child) child))
           children-to-remove)))))
 
  (define rewrite-add
    (lambda (l e)
      ;;; Before adding the element ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes of the list are in evaluation,...
        (throw-exception
         "Cannot add list element; "
         "There are attributes in evaluation."))
      (unless (node-list-node? l) ; ...indeed a list is given as context and...
        (throw-exception
         "Cannot add list element; "
         "The given context is no list-node."))
      (unless (valid-list-element-candidate? l e) ; ...the new element fits.
        (throw-exception
         "Cannot add list element; "
         "The new element does not fit."))
      ;;; When all rewrite constraints are satisfied,...
      (for-each ; ...flush all attribute cache entries influenced by the list-node's number of children and...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 2))
        (node-cache-influences l)))
      (for-each ; ...all entries depending on the new element being a root. Afterwards,...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 1))
        (node-cache-influences e)))
      (node-children-set! l (append (node-children l) (list e))) ; ...add the new element,...
      (node-parent-set! e l)
      (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
      (when (node-parent l)
        (update-inherited-attribution e)))) ; ...any inherited attributes defined for its new context.
 
  (define rewrite-subtree
    (lambda (old-fragment new-fragment)
      ;;; Before replacing the subtree ensure, that no attributes of the old fragment are in evaluation and...
      (when (evaluator-state-in-evaluation? (node-evaluator-state old-fragment))
        (throw-exception
         "Cannot replace subtree; "
         "There are attributes in evaluation."))
      (unless (valid-replacement-candidate? old-fragment new-fragment) ; ...the new fragment fits in its context.
        (throw-exception
         "Cannot replace subtree; "
         "The replacement does not fit."))
      ;;; When all rewrite constraints are satisfied,...
      (detach-inherited-attributes old-fragment) ; ...delete the old fragment's inherited attribution. Then,...
      ; ...flush all attribute cache entries cross-depending the old fragment and...
      (flush-inter-fragment-dependent-attribute-cache-entries old-fragment #t)
      (for-each ; ...all entries depending on the new fragment being a root. Afterwards,...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 1))
        (node-cache-influences new-fragment)))
      (distribute-evaluator-state (node-evaluator-state old-fragment) new-fragment) ; ...update both fragments' evaluator state,...
      (distribute-evaluator-state (make-evaluator-state) old-fragment)
      (set-car! ; ...replace the old fragment by the new one and...
       (list-tail (node-children (node-parent old-fragment)) (- (node-child-index? old-fragment) 1))
       new-fragment)
      (node-parent-set! new-fragment (node-parent old-fragment))
      (node-parent-set! old-fragment #f)
      (update-inherited-attribution new-fragment) ; ...update the new fragment's inherited attribution. Finally,...
      old-fragment)) ; ...return the removed old fragment.
 
  (define rewrite-insert
    (lambda (l i e)
      ;;; Before inserting the new element ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes of the list are in evaluation,...
        (throw-exception
         "Cannot insert list element; "
         "There are attributes in evaluation."))
      (unless (node-list-node? l) ; ...indeed a list is given as context,...
        (throw-exception
         "Cannot insert list element; "
         "The given context is no list-node."))
      (when (or (< i 1) (> i (+ (length (node-children l)) 1))) ; ...the list has enough elements and...
        (throw-exception
         "Cannot insert list element; "
         "The given index is out of range."))
      (unless (valid-list-element-candidate? l e) ; ...the new element fits.
        (throw-exception
         "Cannot add list element; "
         "The new element does not fit."))
      ;;; When all rewrite constraints are satisfied...
      (for-each ; ...flush all attribute cache entries influenced by the list's number of children. Further,...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 2))
        (node-cache-influences l)))
      (for-each ; ...for each tree spaned by the successor element's of the insertion position,...
       ; ...flush all attribute cache entries depending on, but still outside of, the respective tree. Then,...
       (lambda (successor)
         (flush-inter-fragment-dependent-attribute-cache-entries successor #f))
       (list-tail (node-children l) (- i 1)))
      (for-each ; ...flush all attribute cache entries depending on the new element being a root. Afterwards,...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 1))
        (node-cache-influences e)))
      (cond ; ...insert the new element,...
        ((null? (node-children l))
         (node-children-set! l (list e)))
        ((= (length (node-children l)) (- i 1))
         (node-children-set! l (append (node-children l) (list e))))
        (else
         (let ((insert-head (list-tail (node-children l) (- i 1))))
           (set-cdr! insert-head (cons (car insert-head) (cdr insert-head)))
           (set-car! insert-head e))))
      (node-parent-set! e l)
      (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
      (when (node-parent l)
        (update-inherited-attribution e)))) ; ...any inherited attributes defined for its new context.
 
  (define rewrite-delete
    (lambda (n)
      ;;; Before deleting the element ensure, that...
      (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation and...
        (throw-exception
         "Cannot delete list element; "
         "There are attributes in evaluation."))
      (unless (and (node-parent n) (node-list-node? (node-parent n))) ; ...the given node is element of a list.
        (throw-exception
         "Cannot delete list element; "
         "The given node is not element of a list."))
      ;;; When all rewrite constraints are satisfied, flush all attribute cache entries influenced by...
      (for-each ; ...the number of children of the list node the element is part of. Further,...
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 2))
        (node-cache-influences (node-parent n))))
      (detach-inherited-attributes n) ; ...delete the element's inherited attributes and...
      (flush-inter-fragment-dependent-attribute-cache-entries n #t) ; ...the attribute cache entries cross-depending its subtree...
      (for-each ; ...and for each tree spaned by its successor elements,...
       ; ...flush all attribute cache entries depending on, but still outside of, the respective tree. Then,...
       (lambda (successor)
         (flush-inter-fragment-dependent-attribute-cache-entries successor #f))
       (list-tail (node-children (node-parent n)) (node-child-index? n)))
      (node-children-set! (node-parent n) (remq n (node-children (node-parent n)))) ; ...remove the element from the list,...
      (node-parent-set! n #f)
      (distribute-evaluator-state (make-evaluator-state) n) ; ...reset its evaluator state and...
      n)) ; ...return it.
 
  (define perform-rewrites
    (lambda (n strategy . transformers)
      (define root
        (let loop ((n n))
          (if (ast-has-parent? n)
              (loop (ast-parent n))
              n)))
      (define root-deleted/inserted?
        (let ((evaluator-state (node-evaluator-state root)))
          (lambda ()
            (not (eq? evaluator-state (node-evaluator-state root))))))
      (define find-and-apply
        (case strategy
          ((top-down)
           (lambda (n)
             (and
              (not (node-terminal? n))
              (or
               (find (lambda (transformer) (transformer n)) transformers)
               (find find-and-apply (node-children n))))))
          ((bottom-up)
           (lambda (n)
             (and
              (not (node-terminal? n))
              (or
               (find find-and-apply (node-children n))
               (find (lambda (transformer) (transformer n)) transformers)))))
          (else (throw-exception
                 "Cannot perform rewrites; "
                 "Unknown " strategy " strategy."))))
      (let loop ()
        (when (root-deleted/inserted?)
          (throw-exception
           "Cannot perform rewrites; "
           "A given transformer manipulated the root of the AST."))
        (let ((match (find-and-apply root)))
          (if match
              (cons match (loop))
              (list)))))))
