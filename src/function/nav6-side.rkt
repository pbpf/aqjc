#lang racket
(require "private/database.rkt"
         "private/preferences.rkt"
         "private/main-frame.rkt"
         "private/list-box.rkt"
         "private/pdf-render.rkt"
         "private/active-unit.rkt"
         "nav6-base.rkt"
         (prefix-in page1: "nav6-page1.rkt")
         (prefix-in page2: "nav6-page2.rkt")
         "tools.rkt"
         racket/gui/base)
(define side-color  '( "gray" "CornflowerBlue"))
(define abutton% (active-button-mixin% button%))
(define @1 (new abutton%[parent nv-side]
           [label "安全形势分析"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 61)
                                     
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 61)
                                     (show-page1)
                                     (page1:init!)
                                     )]))
(define @2 (new abutton%[parent nv-side]
           [label "免责报告填写系统"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 62)
                                     ;(send file-viewer reset)
                                     ;(reset-enable)
                                     (send page2:viewer-file-list reload* 62)
                                     (show-page2)
                                     (page2:init!)
                                     )]))
(define @3 (new abutton%[parent nv-side]
           [label "安全风险评估"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 63)
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 63)
                                     (show-page1)
                                     (page1:init!))]))
(define @4 (new abutton%[parent nv-side]
           [label "照相管理"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 64)
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 64)
                                     (show-page1)
                                     (page1:init!))]))
(define @5 (new abutton%[parent nv-side]
           [label "质量检验"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 65)
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 65)
                                     (show-page1)
                                     (page1:init!))]))
(define @6 (new abutton%[parent nv-side]
           [label "安全奖惩"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 66)
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 66)
                                     (show-page1)
                                     (page1:init!))]))
(define @7 (new abutton%[parent nv-side]
           [label "安全征文"][colors side-color]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! 67)
                                     (send page1:file-viewer reset)
                                     ;(reset-enable)
                                     (send page1:viewer-file-list reload* 67)
                                     (show-page1)
                                     (page1:init!))]))
