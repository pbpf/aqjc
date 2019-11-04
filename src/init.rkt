#lang racket/gui
(require "function/private/database.rkt"
         db/base
         db/postgresql)

(define(configure)
  (define page1 (new frame%[label "服务端初始化"][width 300]))
  (define v (new vertical-panel%[parent page1][spacing 10][vert-margin 20]))
  (define server(new text-field% [label "服务器地址"][parent v][init-value "127.0.0.1"]))
  (define user (new text-field% [label "超级用户名"][parent v][init-value "postgres"]))
  (define pass (new text-field% [label "密         码"][parent v][style '(single password)]))
  (define h (new horizontal-panel% [parent v][alignment '(center center)][spacing 20]))
  (define(login-base-datebase server user pass)
  (with-handlers([exn:fail:network:errno? (lambda(e)(message-box "错误" "用户名密码错误"
                                                        page1 '(ok stop))
                              #f)]
                 [exn:fail? (lambda(e)(message-box "错误" (exn-message e)
                                                        page1 '(ok stop))
                              #f)])
  (postgresql-connect #:user user
                      #:server server
                      #:database "postgres"
                      #:password pass)))
  (define init (new button% [label "初始化"][parent h]
                    [callback(lambda(b e)
                               (define ok? (message-box "警告" "初始化数据库将会丢失全部数据!确定要这样做吗?"
                                                        page1 '(yes-no stop)))
                               (when (eq? ok? 'yes)
                                 (define cnn (login-base-datebase (send server get-value)
                                                                  (send user get-value)
                                                                  (send pass get-value)))
                                 (when cnn   (define ok?(with-handlers([exn:fail:network:errno? (lambda(e)(message-box "错误" (exn-message e)
                                                                                                            page1 '(ok stop))#f)]
                                                            [exn:fail? (lambda(e)(message-box "错误" (exn-message e)
                                                                                              page1 '(ok stop))#f)])
                                                          (init-database!  cnn (send server get-value)
                                                                           (send user get-value)
                                                                           (send pass get-value))))
                                   (message-box "通知" (if ok? "初始化数据库完成" "初始化数据库失败")
                                                        page1 '(ok))
                                   (send page1 show #f))))]))
  (define cancle (new button% [label "取消"][parent h][callback(lambda(b e)(send page1 show #f))]))
  (send page1 show #t))
(configure)