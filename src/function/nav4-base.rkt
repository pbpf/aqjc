#lang racket/base
(require "private/main-frame.rkt"
         "private/pdf-render.rkt"
         "private/active-unit.rkt"
         racket/gui/base
         racket/class
         framework)
(provide nv nv-side nv-info nv-tools file-list-and-viewer file-panel file-list-panel show-file-viewer view-or-edit file-viewer editer editer-box)
(send main-frame begin-container-sequence)
(define nv(new horizontal-panel%[parent view-panel]))
(define nv-side (new (active-panel-mixin% vertical-panel%) [parent nv][enabled #f]
                        [style '( border)]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define nv-info (new (panel:single-mixin panel%)
                       ; [label "info"]
                        [parent nv]
                        [style '( border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))

(define file-list-and-viewer(new panel:horizontal-dragable%  [parent nv-info][min-height 500][stretchable-height #t]))
(define file-panel(new panel%[parent file-list-and-viewer]))
(define file-list-panel (new vertical-panel% [parent file-panel][min-width 495]))
(define nv-tools(new horizontal-panel%[parent file-list-panel][stretchable-height #f][enabled #f]))

(define view-or-edit(new (panel:single-mixin panel%)[parent file-list-and-viewer]))
(define file-viewer (new pdf-viewer-panel%
                    [parent view-or-edit]
                     [style '(border )]
                    ))
(define editer (new panel%[parent view-or-edit]))
(define editer-box (new vertical-panel%[parent editer][style '(auto-vscroll)]))
(define(hide-file-viewer)
  ;(send file-list-and-viewer show #f)
  (send file-list-panel reparent  nv-info)
  (send nv-info active-child file-list-panel)
  )
(define(show-file-viewer)
  (send file-list-panel reparent  file-panel)
   (send nv-info active-child file-list-and-viewer)
  ;(send file-list-and-viewer show #t)
  )
;(hide-file-viewer)

(send main-frame end-container-sequence)