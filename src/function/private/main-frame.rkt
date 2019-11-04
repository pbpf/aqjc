#lang racket/base
(require racket/gui/base
         framework
         racket/class
        ; "panel-control.rkt"
         "preferences.rkt"
         "active-unit.rkt")

(provide main-frame nav-panel view-panel)

(define main-frame(new frame% [label  main-frame-label]
                       [min-width main-frame-min-width]
                       [min-height 300]
                       ))

(define main-panel (new vertical-panel% [parent main-frame]
                        ;[style '(transparent)]
                        ))

(define nav-panel  (new (active-panel-mixin% horizontal-panel%)[parent main-panel]
                        [style '( border)]
                        [alignment '(center center)]
                        [stretchable-height #f]))

(define view-panel (new (panel:single-mixin  panel%) [parent main-panel][style '(border)]))
#|
(define side-panel (new (panel-control-mixin panel%) [parent view-panel]
                        [style '( border)]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define info-panel (new (panel-control-mixin panel%)
                       ; [label "info"]
                        [parent view-panel]
                        [style '(vscroll border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))
|#