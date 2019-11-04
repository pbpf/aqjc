#lang racket/base
(require racket/gui/base
         racket/class
         "function/private/main-frame.rkt"
         "function/private/active-unit.rkt"
         "function/private/preferences.rkt"
       ;  "function/side.rkt"
       ;  "function/nav3.rkt"
         
         "function/nav1.rkt"
         "function/nav2.rkt"
         "function/nav3.rkt"
         "function/nav4.rkt"
         "function/nav5.rkt"
         "function/nav6.rkt"
         "function/nav7.rkt"
         "function/nav8.rkt"
         "function/tools.rkt"
         ;"function/viewer.rkt"
         )
(define abutton% (active-button-mixin% button%))
(define nav-color  '( "gray" "CornflowerBlue"))
(send nav-panel begin-container-sequence)
(define nav1 (new 	abutton% [label "运行规范"][parent nav-panel][horiz-margin 0][colors nav-color]
                    [min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv1)
                               ; (reload-nv1)
                                (send main-frame end-container-sequence)
                                )]))

(define nav2 (new 	abutton% [label "工作计划"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv2)
                                (send main-frame end-container-sequence))])
                    )

(define nav3 (new 	abutton% [label "监察实施"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                             ; (displayln "监察实施")
                              (send main-frame begin-container-sequence)
                               (send view-panel active-child  nv3)
                               (set-current-subindex! 30)
                               (reload-nv3)
                               (init3!)
                               (send main-frame end-container-sequence)
                                )]))

(define nav4 (new 	abutton% [label "分析预测"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv4)
                                (set-current-subindex! 40)
                                (reload-nv4)
                                (init4!)
                                (send main-frame end-container-sequence)
                                )]))
(define nav5 (new 	abutton% [label "总结报告"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv5)
                                (send main-frame end-container-sequence))])
                    )
(define nav6 (new 	abutton% [label "专项工作"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv6)
                                (send main-frame end-container-sequence))])
                    )
(define nav7 (new   abutton% [label "教育宣传"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv7)
                                (send main-frame end-container-sequence))])
                    )
(define nav8 (new   abutton% [label "用户系统"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width][colors nav-color]
                    [callback (lambda( b e)
                                (send main-frame begin-container-sequence)
                                (send view-panel active-child  nv8)
                                (send main-frame end-container-sequence))])
                    )


#|
(define nav4 (new 	button% [label "分析预测"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)(send side-panel show-child
                                                 nav4-side))]))
(define nav5 (new 	button% [label "总结报告"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)(send side-panel show-child
                                                 nav5-side))]))
(define nav6 (new 	button% [label "专项工作"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)(send side-panel show-child
                                                 nav6-side))]))
(define nav7 (new 	button% [label "教育宣传"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)(send side-panel show-child
                                                 nav7-side))]))
(define nav8 (new 	button% [label "用户系统"][parent nav-panel][horiz-margin 0][min-height nav-button-height][min-width nav-button-width]
                    [callback (lambda( b e)(send side-panel show-child
                                                 nav8-side))]))
|#

(send nav-panel end-container-sequence)