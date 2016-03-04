#lang racket/gui

(require racket/gui/base)
(require racket/draw)

(provide create-file-select-dialog)
(provide create-mathing-commands-dialog)


(define create-file-select-dialog
  (lambda (_frame _title _butt-label _must-exists _callback-class _type)
    (define _curr-dir (path->string (current-directory)))
    (define _dialog null)
    (define _list-box null)
    (define _path-field null)
    (define _fname-field null)
    (define _dirs-num 0)
    
    (define (prepare-file _fname)
      (let* ((_len (string-length _fname)) (_ext (substring _fname (- _len 4) _len)))
        (if (string=? _ext ".rgd")
            _fname
            (string-append _fname ".rgd"))))
    
    (define (is-alloved-file? _fname)
      (let ((_len (string-length _fname)))
        (and (> _len 4) (string=? ".rgd" (substring _fname (- _len 4) _len)))))
        
    (define (open-dir _dir)
      (if (string=? _dir "..")
          (let ((_list (string-split _curr-dir "/")))
            (set! _curr-dir (string-append "/" (string-join (reverse (cdr (reverse _list))) "/") "/")))
          (set! _curr-dir (string-append _curr-dir _dir "/")))
      (read-curr-dir))
      
    (define (select-file _fname)
      (let ((_tmp (string-append _curr-dir (substring _fname 2 (string-length _fname)))))
        (cond ((directory-exists? (string-append _curr-dir _fname))
               (open-dir _fname))
              ((not _must-exists)
               (cond ((= _type 1) (send _callback-class open-project (prepare-file _tmp)))
                     ((= _type 2) (send _callback-class save-project (prepare-file _tmp)))
                     ((= _type 3) (send _callback-class load-project (prepare-file _tmp))))
               (send _dialog on-exit))
              ((and (file-exists? _tmp) (is-alloved-file? _tmp))
               (cond ((= _type 1) (send _callback-class open-project _tmp))
                     ((= _type 2) (send _callback-class save-project _tmp))
                     ((= _type 3) (send _callback-class load-project _tmp)))
               (send _dialog on-exit))
              (else
               (message-box "No file!" (string-append "Selected file '" _tmp "' does not exist!"))))))
    
    (define (read-curr-dir)
      (define _dirs-list (list))
      (define _files-list (list))
      (define qsorts
        (lambda (l)
          (if (null? l)
              '()
              (append
               (qsorts (filter (lambda (x) (string<=? x (car l))) (cdr l)))
               (list (car l))
               (qsorts (filter (lambda (x) (string>? x (car l))) (cdr l)))))))
        
      (let ((_items (directory-list (string->path _curr-dir))) (_choices '()))
        (let loop ((_list _items)) ;divide entries into directories and files
          (cond ((not (null? _list))
                 (let* ((_fname (path->string (car _list))) (_path (string-append _curr-dir _fname)))
                   (cond ((directory-exists? _path)
                          (set! _dirs-list (append _dirs-list (list _fname))))
                         ((and (file-exists? _path) (is-alloved-file? _fname))
                          (set! _files-list (append _files-list (list (string-append "- " _fname)))))
                         (else
                          '()))
                   (loop (cdr _list))))))
        (set! _dirs-num (length _dirs-list))
        (set! _choices (append (list "..") (append (qsorts _dirs-list) (qsorts _files-list))))
        (send _list-box set _choices)
        (let ((_len (string-length _curr-dir)))
          (if (> _len 60)
              (send _path-field set-value (string-append "..." (substring _curr-dir (- _len 60) _len)))
              (send _path-field set-value _curr-dir)))))
    
    (set! _dialog (new frame% (parent _frame) (label _title) (width 600) (stretchable-width #f) (height 500) (stretchable-height #f)))
    (let* ((_main-panel (new vertical-panel% (parent _dialog))) (_panel (new vertical-panel% (parent _main-panel))))
      (set! _path-field (new text-field% (parent _panel) (label "Path: ") (enabled #f)))
      (set! _list-box (new list-box% (parent _panel) (label "") (choices (list)) (min-width 500) (min-height 300)
                           (callback (lambda (button event)
                                       (let ((_selection (send _list-box get-selection)))
                                         (cond (_selection
                                                (let ((_str (send _list-box get-string _selection)))
                                                  (let ((_sig (send event get-event-type)))
                                                    (cond ((eq? _sig 'list-box-dclick)
                                                           (select-file _str))
                                                          ((eq? _sig 'list-box)
                                                           (cond ((is-alloved-file? _str)
                                                                  (send _fname-field set-value (substring _str 2 (string-length _str))))))))))))))))
      (let* ((_hpanel (new horizontal-panel% (parent _panel))))
        (set! _fname-field (new text-field% (parent _hpanel) (label "File name: ")))
        (new button% (parent _hpanel) (label _butt-label) (callback (lambda (button event) (select-file (string-append "- " (send _fname-field get-value)))))))
      (read-curr-dir))
    (send _dialog show #t)
    _dialog))


(define create-mathing-commands-dialog
  (lambda (_frame _text)
    (define _dialog null)
    (define _fun-field null)
    
    (set! _dialog (new frame% (parent _frame) (label "The list with the matching commands")))
    (let* ((_main-panel (new vertical-panel% (parent _dialog))) (_panel (new vertical-panel% (parent _main-panel))))
      (set! _fun-field (new text-field% (parent _panel) (label "") (init-value _text) (style (list 'multiple)) (min-width 650) (min-height 450))))
    (send _dialog show #t)
    _dialog))
