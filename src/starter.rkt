#lang racket/base
(require "function/private/main-frame.rkt"
         "nav.rkt"
       ; "function.rkt"
         racket/class
         )
(provide startup)
;(send side-panel change-children (lambda(x)nav1-list))
;(send nav1-1-file-list-panel stretchable-height 30)
(define(type->labelinfo t)
  (case t
    ([0](values "公共用户" "只读权限"))
    ([1](values "普通用户" "部分编辑权限"))
    ([2](values "超级用户" "完全可编辑权限"))
    (else (values "所有者" "最终权限"))))
(define(startup username type)
  (define-values(a b)(type->labelinfo type))
  (send main-frame set-label (format "安全监察精细化管理系统——~a[~a]已登录,~a" username a b))
  (send main-frame show #t))