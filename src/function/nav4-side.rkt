#lang racket/base
(require "private/preferences.rkt"
         "private/main-frame.rkt"
         "private/active-unit.rkt"
         "nav4-base.rkt"
         "nav4-page.rkt"
        ; "side.rkt"
         racket/gui/base
         racket/class)
(define side-color  '( "gray" "CornflowerBlue"))
(define abutton% (active-button-mixin% button%))
(define @x1 (new abutton%[parent nv-side][colors side-color]
           [label "问题汇总"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p1))]))
(define @x2 (new abutton%[parent nv-side][colors side-color]
           [label "量化分析"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p2))]))
(define @x3 (new abutton%[parent nv-side][colors side-color]
           [label "形势分析"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p3))]))
(define @x4 (new abutton%[parent nv-side][colors side-color]
           [label "研究措施"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p4))]))
(define @x5 (new abutton%[parent nv-side][colors side-color]
           [label "安全预警"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p5))]))