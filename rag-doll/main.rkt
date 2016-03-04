#lang racket/gui

(require racr/core)
(require rag-doll/util)
(require rag-doll/base)
(require rag-doll/lib/gui/rag-doll-connector)
(require rag-doll/lib/gui/lists/references-list)
(require rag-doll/lib/gui/lists/types-list)
(require rag-doll/lib/gui/lists/names-list)
(require rag-doll/lib/gui/gui-menu)
(require rag-doll/lib/gui/windows)
(require rag-doll/lib/gui/gui)
(require compatibility/mlist)
(require racket/place/distributed)

(define RagDoll%
  (class object%
    
    (field (src-path null))
    (field (win-list null))
    (field (user-spec null))
    (field (id 1))
    (field (def-width 800))
    (field (def-height 600))
    (field (cfg-file-data '()))

    
    (define/public (init _user-spec)
      (if (or (null? _user-spec) (not (= (specification->phase _user-spec) 3)))
          (message-box "Error!" "The specyfication must be compailed!")
          (begin
            (set! user-spec _user-spec)
            (prepare-paths)
            (read-config-file)
            (new-project))))

    
    (define/private (prepare-paths)
      (set! src-path (string-append (path->string (collection-path "rag-doll")) "/src/")))

    
    (define/private (update-windows-list)
      (set! win-list (let loop ((_l win-list))
                       (if (null? _l)
                           null
                           (let ((_id (mlist-ref (car _l) 0)) (_win (mlist-ref (car _l) 1)))
                             (append
                              (if (close-if-not-shown _id)
                                  null
                                  (list (mlist _id _win (send _win get-opened-file))))
                              (loop (cdr _l))))))))

    
    (define/public (get-opened-files)
      (update-windows-list)
      (let loop ((_l win-list))
        (if (null? _l)
            null
            (mappend
             (mlist (mlist-ref (car _l) 2))
             (loop (cdr _l))))))


    (define/public (new-project)
      (open ""))

    
    (define/private (is-file-opened? _fpath)
      (let loop ((_l win-list) (_i 0))
        (if (null? _l)
            -1
            (if (string=? (mlist-ref (car _l) 2) _fpath)
                (mlist-ref (car _l) 0)
                (loop (cdr _l) (+ _i 1))))))
    
    
    (define/private (get-win _id)
      (let loop ((_l win-list) (_i 0))
        (if (null? _l)
            '()
            (if (= (mlist-ref (car _l) 0) _id)
                (mlist-ref (car _l) 1)
                (loop (cdr _l) (+ _i 1))))))
    
    
    (define/public (open _fpath)
      (update-windows-list)
      (define _pos (if (string=? _fpath "") -1 (is-file-opened? _fpath)))
      (cond ((and (> _pos -1) (not (close-if-not-shown _pos)))
             (message-box "Warning!" (string-append "File '" _fpath "' is already opened!")))
            (else
             (let ((_win (new RagDollWin% (win-manager this) (win-width def-width) (win-height def-height) (default-data cfg-file-data) (src-path src-path))))
               (set! id (+ id 1))
               (set! win-list (append win-list (list (mlist id _win _fpath))))
               (send _win gui-init this user-spec id)
               (cond ((and (not (string=? _fpath "")) (not (send _win load-state _fpath)))
                      (close id)
                      (set! id (- id 1))))))))
        
    
    (define/public (can-load? _fpath)
      (define _pos (is-file-opened? _fpath))
      (cond ((and (> _pos -1) (not (close-if-not-shown _pos)))
             (message-box "Warning!" (string-append "File '" _fpath "' is already opened!"))
             #f)
            (else
             #t)))
        
    
    (define/private (close-if-not-shown _id)
      (let ((_win (get-win _id)))
        (if (and (not (null? _win)) (not (send _win is-shown?)))
            (close _id)
            #f)))
    
    
    (define/private (update-path _id _fpath)
      (set! win-list (let loop ((_l win-list))
                       (if (null? _l)
                           null
                           (append
                            (if (= (mlist-ref (car _l) 0) _id)
                                (list (mlist _id (mlist-ref (car _l) 1) _fpath))
                                (list (car _l)))
                            (loop (cdr _l)))))))
      
    
    (define/public (load _id _fpath)
      (update-path _id _fpath))

    
    (define/public (close _id)
      (let ((len (length win-list)))
        (set! win-list (let loop ((_l win-list))
                         (if (null? _l)
                             null
                             (append
                              (if (= (mlist-ref (car _l) 0) _id)
                                  (list)
                                  (list (car _l)))
                              (loop (cdr _l))))))
        (< (length win-list) len)))

    
    (define/public (save _id _fpath)
      (define _pos (is-file-opened? _fpath))
      (define _ret (not (and (> _pos -1) (not (close-if-not-shown _pos)) (not (= _pos _id)))))
      (if _ret
          (update-path _id _fpath)
          (message-box "Warning!" (string-append "You can not save the project into the '" _fpath "'\n The file is opened in other window!")))
      _ret)

    
    (define read-config-file
      (lambda ()
        (define _fh '())
        
        (define (read-line _fh)
          (let ((_ch (read-char _fh)))
            (cond ((eof-object? _ch)
                   #f)
                  ((equal? _ch #\newline)
                   "")
                  (else
                   (string-append (list->string (list _ch)) (read-line _fh))))))
        
        (define (_read-config-file)
          (let ((line (read-line _fh)))
            (cond (line
                   (set! line (string-replace line " " ""))
                   (let ((_name "") (_val "") (_pos (string-pos line #\:)))
                     (cond ((> _pos -1)
                            (set! _name (substring line 0 _pos))
                            (set! _val (substring line (+ _pos 1)))
                            (cond ((string=? _name "width")        (set! def-width (string->number _val)))
                                  ((string=? _name "height")       (set! def-height (string->number _val)))
                                  ((string=? _name "def_ref_name") (set! cfg-file-data (append cfg-file-data (list _val)))))
                     (_read-config-file))))))))
        
        (set! _fh (open-input-file (string-append src-path "main.cfg")))
        (_read-config-file)
        #t))

    
    (super-new)))

(provide RagDoll%)


