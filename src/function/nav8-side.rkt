#lang racket/base
(require "nav8-base.rkt"
         "nav8-page1.rkt"
         "nav8-page2.rkt"
         "private/preferences.rkt"
         "private/active-unit.rkt"
         "private/database.rkt"
         "tools.rkt"
         racket/class
         racket/gui/base
         net/sendurl
         net/url
         racket/runtime-path
         )
(define-runtime-path docpath "../doc")
(define side-color  '( "gray" "CornflowerBlue"))
(define abutton% (active-button-mixin% button%))
(define user (new abutton%
                  [parent nv-side][colors side-color]
                  [min-height side-button-height]
                  [min-width  side-button-width]
                  [label "用户管理"]
                  [callback (lambda(b e)(send nv-info active-child page1)
                              (load-data (get-cnn))
                              (init!))]))
(define backup (new abutton%
                  [parent nv-side][colors side-color]
                  [min-height side-button-height]
                  [min-width  side-button-width]
                  [label "备份还原"]
                  [callback (lambda(b e)(send nv-info active-child page2)
                              ;(load-data (get-cnn))
                              (page2-load-data!))]))
(define info (new abutton%
                  [parent nv-side][colors side-color]
                  [min-height side-button-height]
                  [min-width  side-button-width]
                  [label "文档说明"]
                  [callback (lambda(b e)(send-url (url->string (path->url(build-path docpath "index.html")))))]))