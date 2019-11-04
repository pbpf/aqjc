#lang racket/base
(require "writing-word.rkt"
         racket/gui/base
         racket/class
         racket/file
          file/convertible
          file/sha1
          "../tools.rkt"
         "table.rkt")
(provide list-control:table-cell-mixin
         text-field:table-cell-mixin
         label:table-cell-mixin
         list-control:table-cell-yes-no-mixin
         word-table-cell-mixin
         canvas:table-cell-mixin
         control:virtual-table-cell-mixin
         panel-table-cell-mixin
         table-cell-mixin)

(define value-cell<%> (interface() get-string-value get-type meta-edit-able?))
(define meta-cell<%> (interface() get-meta-value set-meta-value ))
(define text-field<%> (class->interface text-field%));(interface()get-value))
(define yes-no<%>(interface()select-yes?))
(define meta-value-cell<%> (interface()get-pos get-merge after-insert-row after-delete-row))
(define word-value-cell<%> (interface(meta-value-cell<%>)get-word-pos ));after-insert-line

    
                           
;table-cell-mixin control<%> -> table cell writer  text-field%(
(define list-control:table-cell-mixin
  (mixin (list-control<%>)(value-cell<%> meta-cell<%>)
    (super-new [label #f])
    (define/public(get-type)
      'text)
    (define/public(meta-edit-able?)
      #t)
    (define/public(get-meta-value)
      (send this get-selection))
    (define/public(set-meta-value n)
      (send this set-selection n))
    (define/public(get-string-value)
      (send this  get-string-selection))))
(define text-field:table-cell-mixin
  (mixin (text-field<%>)(value-cell<%> meta-cell<%>)
    (super-new [label #f])
    (define/public(get-type)
      'text)
    (define/public(meta-edit-able?)
      #t)
      (define/public(get-meta-value)
      (send this get-value))
    (define/public(set-meta-value s)
      (send this set-value s))
    (define/public(get-string-value)
      (send this  get-value))))
(define label:table-cell-mixin
  (mixin (control<%>)(value-cell<%>)
    (super-new)
    (define/public(get-type)
      'text)
    (define/public(meta-edit-able?)
      #f)
    (define/public(get-string-value)
      (send this  get-label))))
(define control:virtual-table-cell-mixin
  (mixin (control<%>)(value-cell<%>)
    (super-new)
    (define/public(get-type)
      'virtual)
    (define/public(meta-edit-able?)
      #f)
    (define/public(get-string-value)
      "")))

(define list-control:table-cell-yes-no-mixin
  (mixin (list-control<%>)(value-cell<%> yes-no<%> meta-cell<%>)
    (super-new [choices '( "否" "是")][label #f])
    (define/public(get-type)
      'text)
    (define/public(meta-edit-able?)
      #t)
    (define/public(get-meta-value)
      (send this get-selection))
    (define/public(set-meta-value n)
      (send this set-selection n))
    (define/public(get-string-value)
      (send this  get-string-selection))
    (define/public(select-yes?)
      (define t (send this get-selection))
      (and t
           (= t 1)))))

(define canvas:table-cell-mixin
  (mixin (canvas<%>)(value-cell<%> meta-cell<%>)
    (field [path ""][key-map (new keymap%)][bitmap (make-bitmap 100 100)])
    (super-new [paint-callback (lambda(c dc)
                                 (send dc set-smoothing 'aligned)
                                 (define h1 (send c get-height))
                                 (define w1 (send c get-width))
                                 (define h2 (send bitmap get-height))
                                 (define w2 (send bitmap get-width))
                                 (send dc set-scale (/ w1 w2)(/ h1 h2))
                                 (send dc draw-bitmap	bitmap 0 0))])
    
    (define/public(get-type)
      'pict)
    (define/public(get-string-value)
      path)
    
    (define/public(meta-edit-able?)
      #t)
     (define/public(get-meta-value)
      (bytes->hex-string(convert bitmap 'png-bytes )))
    (define/public(set-meta-value bs)
      (load-bitmap (read-bitmap (open-input-bytes (hex-string->bytes bs))))
      (define t (make-temporary-file "rkttmp~a.png" #f cache-path))
      (send bitmap save-file t 'png)
      (set! path (path->string t)))
    (define/public(load-file f)
      (set! path (path->string f))
      (load-bitmap (read-bitmap f)))
    (define/public(load-bitmap t)
      (set! bitmap t)
      (send this refresh))
    (define/public(save-bitmap)
      (void))
    (define/override(on-event e)
     (send key-map handle-mouse-event "leftbuttondouble" e)
     (send key-map  map-function  "leftbuttondouble"  "get-file"))
    (send key-map add-function "get-file"
          (lambda(b e)
            (define t (get-file))
            (when t
              (load-file t))))))

(define panel-table-cell-mixin
  (mixin (area-container<%>)(meta-value-cell<%> value-cell<%> )
    (init-field pos merge)
    (super-new)
   (define/public(get-pos)
      pos)
    (define/public(meta-edit-able?)
      (define t (send this get-children))
      (send (car t) meta-edit-able?))
     (define/public(get-meta-value)
       (define t (send this get-children))
      (send (car t) get-meta-value))
    (define/public(set-meta-value n)
      (define t (send this get-children))
      (send (car t) set-meta-value n))
    (define/public(get-word-pos)
      (send (send this get-parent) pos->word-pos pos))
    (define/public(get-merge)
      merge)
    (define/public(write-table word tab word-pos)
      
      (case (send this get-type)
      [(text)(cell-insert-text! (table-ref tab (pos-row word-pos) (pos-col word-pos))
                                 (send this get-string-value))]
      [(pict);(printf "~a ~a\n" (send this get-string-value) word-pos)
             (unless(string=? (send this get-string-value) "")
              ; (printf "~a ~a\n" (send this get-string-value) word-pos)
               (cell-insert-pict-auto! word (table-ref tab (pos-row word-pos) (pos-col word-pos))  (send this get-string-value)))]
      [else (void)]))
    (define/public(get-string-value)
      (define t (send this get-children))
      (send (car t) get-string-value))
    (define/public(get-type)
      (define t (send this get-children))
      (send (car t) get-type))
    (define/public(after-insert-row p)
      (set! pos (cell-insert-row pos p)))
    (define/public(after-delete-row p)
      (set! pos (cell-del-row pos p)))))
(define table-cell-mixin
  (mixin ()(meta-value-cell<%>)
    (init-field pos merge)
    (super-new)
   (define/public(get-pos)
      pos)
    (define/public(get-word-pos)
      (send (send this get-parent) pos->word-pos pos))
    (define/public(get-merge)
      merge)
    
    (define/public(after-insert-row p)
      (set! pos (cell-insert-row pos p)))
    (define/public(after-delete-row p)
      (set! pos (cell-del-row pos p)))))

(define word-table-cell-mixin
  (mixin (value-cell<%>)(word-value-cell<%>)
    
    (init-field pos merge)
    ;(inherit-field parent)
    (super-new)
    (define/public(get-pos)
      pos)
    (define/public(get-word-pos)
      (send (send this get-parent) pos->word-pos pos))
    (define/public(get-merge)
      merge)
    (define/public(after-insert-row p)
      (set! pos (cell-insert-row pos p)))
    (define/public(after-delete-row p)
      (set! pos (cell-del-row pos p)))
    (define/public(write-table word tab word-pos)
      
      (case (send this get-type)
      [(text)(cell-insert-text! (table-ref tab (pos-row word-pos) (pos-col word-pos))
                                 (send this get-string-value))]
      [(pict)(unless(string=? (send this get-string-value) "")
               ;(printf "~a ~a\n" (send this get-string-value) word-pos)
               (cell-insert-pict-auto! word (table-ref tab (pos-row word-pos) (pos-col word-pos))  (send this get-string-value)))]
      [else (void)]))))