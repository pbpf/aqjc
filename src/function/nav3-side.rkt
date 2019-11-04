#lang racket/base
(require "private/preferences.rkt"
         "private/main-frame.rkt"
         "private/active-unit.rkt"
         "nav3-base.rkt"
         "nav3-page2.rkt"
        ; "side.rkt"
         racket/gui/base
         racket/class)

;(provide nav3-side)
;(define nav3-side (new vertical-panel%[parent side-panel]))
;"建设标准" "岗位职责" "监察单" "法规依据" "在职学习" "岗位认定" "奖惩制度"
(define side-color  '( "gray" "CornflowerBlue"))
(define abutton% (active-button-mixin% button%))
(define @x1 (new abutton%[parent nv-side][colors side-color]
           [label "下达任务"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p1))]))
(define @x2 (new abutton%[parent nv-side][colors side-color]
           [label "监察准备"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p2))]))
(define @x3 (new abutton%[parent nv-side][colors side-color]
           [label "监察开展"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p3))]))
(define @x4 (new abutton%[parent nv-side][colors side-color]
           [label "问题查处"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p4))]))
(define @x5 (new abutton%[parent nv-side][colors side-color]
           [label "取证记录"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p5))]))

(define @x6 (new abutton%[parent nv-side][colors side-color]
           [label "督促整改"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@x p6))]))


(define @x7 (new abutton%[parent nv-side][colors side-color]
           [label "整改反馈通知"]
           [min-height side-button-height][min-width side-button-width]
           [callback (lambda(b e)(scroll@y p7))]))
#|
(for([i (in-list '("下达任务""监察准备""监察开展""问题查处""取证记录""督促整改""整改反馈通知"))]
     [j (in-range 16 23)])
  (new button%[parent nv-side]
           [label i]
           [min-height side-button-height][min-width side-button-width]))
(send nv-side enable #f)
|#