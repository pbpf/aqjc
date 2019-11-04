#lang racket/base
(require "private/main-frame.rkt"
         "private/tools.rkt"
         "private/active-unit.rkt"
         racket/gui/base
         racket/class
         framework)
(provide nv nv-side page1 page2 show-page1 show-page2)
(send main-frame begin-container-sequence)
(define nv(new horizontal-panel%[parent view-panel]))
(define nv-side (new (active-panel-mixin% vertical-panel%) [parent nv]
                        [style '( border)]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define nv-info (new panel:single%
                       ; [label "info"]
                        [parent nv]
                        [style '( border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))
(define(show-page1)
  (send nv-info active-child page1))
(define(show-page2)
  (send nv-info active-child page2))
(define page1 (new panel:single%[parent nv-info]))
(define page2 (new panel:single%[parent nv-info]))


;(hide-file-viewer)

(send main-frame end-container-sequence)