#lang racket/base
(require "private/main-frame.rkt"
         "private/user.rkt"
          "private/active-unit.rkt"
         racket/gui/base
         racket/class
         db/base
         framework)

(provide (all-defined-out))

(define nv(new horizontal-panel%[parent view-panel]))
(define nv-side (new (active-panel-mixin% vertical-panel%) [parent nv]
                        [style '( border)]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define nv-info (new panel:single%
                       ; [label "info"]
                        [parent nv]
                        [style '(border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))
;(define base-panel [new panel:single% [parent test]])
(define page1 (new panel%[parent nv-info]))
(define page2 (new panel%[parent nv-info]))
(define page3 (new panel%[parent nv-info]))
(define page4 (new panel%[parent nv-info]))

;;;;;------------------------------------------------------------
(define list-box-sort-by-header%
  (class list-box%
    [init-field parent columns]
    (super-new [label  #f]
               [choices '()]
               [parent parent]
               [style (list 'multiple 'extended  'vertical-label  'column-headers 'clickable-headers 'reorderable-headers 'variable-columns)]
               [columns (cons "序号" columns)])
    (define/public(load-data lst)
      (send/apply this set
                  (build-list (length lst) (lambda(x)(number->string x)))
                  (for/list ([i (in-range 0 (length columns))])
                    (for/list([j (in-list lst)])
                      (vector-ref j i))))
      (for([i (in-list lst)]
           [j (in-naturals 0)])
        (send this set-data j (vector-ref i 0))))
    ))

