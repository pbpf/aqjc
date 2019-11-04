#lang racket/gui
;active
(define tab-panel-plus-mixin%
  (mixin(area-container<%>) ()
    (init-field callback
                child-table)
    (super-new [callback (lambda(b e)(callback b e)
                           (send main-box active-child (send this get get-selection)))])
    (define main-box (new panel%[parent this]))
    (define/override(after-new-child c)
      (send c reparent main-box))
    ))