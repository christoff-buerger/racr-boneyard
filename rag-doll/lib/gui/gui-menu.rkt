#lang racket/gui

(require racr/core)
(require rag-doll/base)
(require compatibility/mlist)

(define GuiMenu%
  (class object%
    ; ----- variables -----
    (field (ctrl-down-butt null))
    (field (names-view-butt null))
    (field (types-view-butt null))
    (field (refs-view-butt null))
    (field (combo-addChild null))
    (field (add-butt null))
    (field (delete-butt null))
    (field (delete-all-butt null))
    (field (left-butt null))
    (field (right-butt null))
    (field (home-butt null))
    (field (cut-butt null))
    (field (copy-butt null))
    (field (paste-butt null))
    (field (tb-nodeText null))
    (field (save-state-butt null))
    (field (load-state-butt null))
    (field (fun-gen-butt null))
    
    
    (define/public (set-ctrl-down-butt _el)   (set! ctrl-down-butt _el))
    (define/public (set-names-view-butt _el)  (set! names-view-butt _el))
    (define/public (set-types-view-butt _el)  (set! types-view-butt _el))
    (define/public (set-refs-view-butt _el)   (set! refs-view-butt _el))
    (define/public (set-combo-addChild _el)   (set! combo-addChild _el))
    (define/public (set-add-butt _el)         (set! add-butt _el))
    (define/public (set-delete-butt _el)      (set! delete-butt _el))
    (define/public (set-delete-all-butt _el)  (set! delete-all-butt _el))
    (define/public (set-left-butt _el)        (set! left-butt _el))
    (define/public (set-right-butt _el)       (set! right-butt _el))
    (define/public (set-home-butt _el)        (set! home-butt _el))
    (define/public (set-cut-butt _el)         (set! cut-butt _el))
    (define/public (set-copy-butt _el)        (set! copy-butt _el))
    (define/public (set-paste-butt _el)       (set! paste-butt _el))
    (define/public (set-tb-nodeText _el)      (set! tb-nodeText _el))
    (define/public (set-save-state-butt _el)  (set! save-state-butt _el))
    (define/public (set-load-state-butt _el)  (set! load-state-butt _el))
    (define/public (set-fun-gen-butt _el)     (set! fun-gen-butt _el))
    
    
    (define/public (set-ctrl-down-butt-label _val)   (send ctrl-down-butt set-label _val))
    (define/public (set-types-view-butt-label _val)  (send types-view-butt set-label _val))
    (define/public (set-refs-view-butt-label _val)   (send refs-view-butt set-label _val))
    (define/public (set-tb-nodeText-label _val)      (send tb-nodeText set-label _val))
    (define/public (set-names-view-butt-label _val)  (send names-view-butt set-label _val))
    (define/public (set-combo-addChild-value _val)   (send combo-addChild set-value _val))
    (define/public (set-tb-nodeText-value _val)      (send tb-nodeText set-value _val))

    (define/public (get-tb-nodeText-value)     (send tb-nodeText get-value))
    (define/public (get-combo-addChild-value)  (send combo-addChild get-value))

    (define/public (refresh-tb-nodeText)                  (send tb-nodeText refresh))
    (define/public (enable-combo-addChild _val)           (send combo-addChild enable _val))
    (define/public (update-combo-addChild-choices _list)  (send combo-addChild update-choices _list))


    (define/public (enable-save-butt _b)
      (send save-state-butt enable _b))
    
    
    (define/public (activate-base-gui-menu _b)
      (send combo-addChild enable _b)
      (send add-butt enable _b)
      (send paste-butt enable _b)
      (send tb-nodeText set-value ""))
    
    
    (define/public (activate-node-gui-menu _b)
      (send combo-addChild enable _b)
      (send add-butt enable _b)
      (send copy-butt enable _b)
      (send cut-butt enable _b)
      (send home-butt enable _b)
      (send left-butt enable _b)
      (send right-butt enable _b)
      (cond (_b
             (send delete-butt set-label (read-bitmap "src/i/delete.png"))))
      (send delete-butt enable _b)
      (send delete-all-butt enable _b)
      (send paste-butt enable _b)
      (send tb-nodeText enable _b)
      )
    
    
    (define/public (activate-ref-gui-menu _b)
      (send combo-addChild enable #f)
      (send add-butt enable #f)
      (send copy-butt enable #f)
      (send cut-butt enable #f)
      (send home-butt enable #f)
      (send left-butt enable #f)
      (send right-butt enable #f)
      (send delete-butt enable _b)
      (cond (_b
             (send delete-butt set-label (read-bitmap "src/i/delete-ref.png"))))
      (send delete-all-butt enable #f)
      (send paste-butt enable #f)
      (send tb-nodeText enable _b)
      )
    
    
    (define/public (set-tb-nodeText-as-modified _b)
      (let* ((_str (send tb-nodeText get-label)) (_len (string-length _str)) (_post ""))
        (set! _post (substring _str (- _len 2) _len))
        (if _b
            (cond ((not (string=? _post "*:"))
                   (send tb-nodeText set-label (string-append (substring _str 0 (- _len 2)) "*:"))))
            (cond ((string=? _post "*:")
                   (send tb-nodeText set-label (string-append (substring _str 0 (- _len 2)) ": ")))))))

  

    (super-new)))

(provide GuiMenu%)
