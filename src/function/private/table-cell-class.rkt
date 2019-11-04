#lang racket/base
(require (prefix-in gui: racket/gui/base)
         racket/class
         "table-cell-mixin.rkt"
         "date-panel.rkt"
         "table.rkt"
         "time-choicer.rkt")

(provide (all-defined-out)
         )


;
(define(make-const pos merge label parent[min-width #f][min-height #f] #:panel-style [panel-style '()] )
  (define t (new  table-cell-panel% [pos pos][merge merge][min-width min-width][min-height min-height][parent parent][style panel-style]))
  (new constant%  [label label][parent t][auto-resize #t])
  t
  )
(define(make-text pos merge  parent #:init [init ""] #:call [call (lambda( b e)(void))] #:style [style '(single)] #:min-height [min-height #f]#:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new text% [parent t][init-value init][callback  call][style style][min-height min-height]))
(define(make-yes/no pos merge parent callback #:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new yes/no% [parent t][callback callback]))
(define(make-pict pos merge parent width height #:panel-style [panel-style '()] #:horiz-margin [horiz-margin 0])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new pict% [parent t][min-width width][min-height height][horiz-margin horiz-margin] ))
(define(make-date pos merge parent #:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new date% [parent t]))
(define(make-time pos merge parent #:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new time% [parent t]))
(define(make-button pos merge parent label callback #:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new button% [parent t][label label][callback callback]))
(define(make-choice pos merge parent choices #:panel-style [panel-style '()])
  (define t (new table-cell-panel% [pos pos][merge merge][parent parent][style panel-style]))
  (new choice% [parent t][choices choices]))
;----------------------------------------------------------------------
(define constant% (label:table-cell-mixin gui:message%))
(define text% (text-field:table-cell-mixin gui:text-field%))
(define yes/no%(list-control:table-cell-yes-no-mixin gui:choice%));
(define pict%(canvas:table-cell-mixin gui:canvas%))
(define choice%(text-field:table-cell-mixin gui:combo-field%))
(define date% (text-field:table-cell-mixin date-choicer%))
(define button%(control:virtual-table-cell-mixin gui:button%))
(define time%(text-field:table-cell-mixin time-range-choicer%))
(define table-cell-panel% (panel-table-cell-mixin gui:panel%))

(define pos%
  (class object%
    [init-field pos]
    (super-new)
    (define/public(get-pos)
      pos)
    (define/public(after-insert-row p)
      (set! pos (cell-insert-row pos p)))
    (define/public(after-delete-row p)
      (set! pos (cell-del-row pos p)))))

(define table-variable-row-manger%
  (class object%
    (super-new)
    (init-field  repeat-proc
                 init-pos
                 parent
                 once-repeat-times;for index
                 once-create-rows
                 index
                )
    (field [lines '()]
           [current-count-row 0]
           [current-count 0])
    (define/public(get-lines)
      lines)
    (define/public(get-count)
      current-count)
    (define(get-init-row)
      (+ current-count-row (pos-row (send init-pos get-pos))))
    (define/public(set-meta-data count)
      (let loop ()
        (when (< current-count count)
          (add)
          (loop))))
    (define/public(get-meta-data)
      current-count)
    (define/public (add)
      (define init-row (get-init-row))
      ;(displayln init-row)
      (for([i (in-range init-row (+ init-row once-create-rows))])
       ; (displayln i)
             (send parent insert-row-after-copy-bef i))
      (set! lines (cons  (repeat-proc current-count  (add1 init-row) once-repeat-times)
                           lines))
       (set! current-count-row (+ current-count-row once-create-rows))
      (set! current-count (+ current-count once-repeat-times)))
    (define/public(sub)
      (define init-row (get-init-row))
    ; (displayln current-count)
      (when (>= current-count (* 2 once-repeat-times))
        (for([j (in-list (car lines))])
        ;  (displayln j)
          (send parent delete-child j))
        (set! lines (cdr lines))
        (set! current-count (- current-count once-repeat-times))
        (set!  current-count-row (- current-count-row once-create-rows))
        (for([i (in-range  init-row  (- init-row current-count-row) -1)])
       ;   (displayln i)
          (send parent delete-row i))
        (send parent  container-flow-modified)
        ))
    ;----------------------------------------------------
    (send parent install-variable-row-manger index this)
    ))
                                            