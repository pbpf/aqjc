#lang racket/base
(require "private/main-frame.rkt"
         "private/active-unit.rkt"
         racket/gui/base
         racket/class
         framework)
(provide nv nv-side nv-info nv-tools file-list-and-viewer file-list-panel show-file-viewer hide-file-viewer)


(send main-frame begin-container-sequence)
(define nv(new horizontal-panel%[parent view-panel]))
(define nv-side (new (active-panel-mixin% vertical-panel%) [parent nv]
                        [style '( border)]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define nv-info (new (panel:single-mixin panel%)
                       ; [label "info"]
                        [parent nv]
                        [style '(border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))

(define file-list-and-viewer(new panel:horizontal-dragable%   [parent nv-info][min-height 500][stretchable-height #t]))
(define file-panel(new panel%[parent file-list-and-viewer]))
(define file-list-panel (new vertical-panel% [parent file-panel][min-width 495]))
(define nv-tools(new horizontal-panel%[parent file-list-panel][stretchable-height #f][enabled #f]))
(send main-frame end-container-sequence)


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
;(send file-list-and-viewer 
;(hide-file-viewer)
;(define file-list-and-tools (new vertical-panel% [parent nv1-info])) 
;(send file-list-and-viewer set-orientation #f)