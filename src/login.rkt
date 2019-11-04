#lang racket/base
(require"function/private/database.rkt"
         "starter.rkt"
         "function/private/tools.rkt"
         racket/gui/base
         racket/date
         racket/list
         racket/runtime-path
         racket/class)
(define-runtime-path logo "res\\login.png")
(define-runtime-path history-file "log\\log.history")
(define-runtime-path error-file "log\\error.log")
(define(write-history server username)
  (call-with-output-file history-file
                          (lambda(out)
                            (fprintf out "[~a]:~a@~a\n" (parameterize([date-display-format 'iso-8601])
                                                        (date->string (current-date) #t))
                                     username server))
                          #:mode 'text
                          #:exists 'append))
(define(read-history file)
  (if(file-exists? file)
     (call-with-input-file file
       (lambda(in)
         (for/list([i (in-lines in)]
                   #:when (regexp-match-exact? #rx"^[[].*?[]]:.*?@.*?$" i))
          ; (displayln i)
           (regexp-match #rx"^[[].*?[]]:(.*?)@(.*?)$" i)))
       #:mode 'text)
     '()))
#|
(error-display-handler (lambda(message exn)
                         (call-with-output-file error-file
                           (lambda(out)
                             (fprintf out "[~a]:message=~a;exn=~a\n" (parameterize([date-display-format 'iso-8601])
                                                        (date->string (current-date) #t))message exn))
                           #:mode 'text
                          #:exists 'append)
                         (message-box "错误"
                                      "发生了一些错误,软件即将退出.\n我们已经记录这些问题,并将在\n合适的时候上传,详细信息请查\n阅error.log文件。"
                                      #f
                                      '(ok stop)
                                      )
                         (exit 1)
                         ))
|#

(define history (read-history history-file))
(define servers (if(null? history) '()
                   (remove-duplicates (map caddr history) string=?)))
(define users (if(null? history) '()
                  (remove-duplicates (map cadr history) string=?)))

(define login-frame (new frame%[label "登录"][width 695][height 500][alignment '(center center)]))
(send login-frame begin-container-sequence)
(define p (new pane% [parent login-frame][alignment '(right bottom)]))

(define p1 (new pane% [parent p][min-width 360][min-height 150][horiz-margin 100][vert-margin 120][stretchable-width #f][stretchable-height #f]))
(define v1 (new  vertical-pane% [parent p1][vert-margin 50][horiz-margin 50][spacing 10]))
(define server (new combo-field%[parent v1][label "服务器"][init-value ""][choices servers]))
(define username (new combo-field%[parent v1][label "用户名"][init-value ""][choices users]))
(define password (new text-field%[parent v1][label "密   码"][init-value ""][style '(single password)]))
(define h1 (new horizontal-pane%[parent v1][spacing 20][alignment '(center center)]))
(define login (new button%[label "登录"][parent h1][callback(lambda(b e)
                                                            (define t(send current-login-context
                                                                           login
                                                                           (send server get-value)
                                                                           (send username get-value)
                                                                           (send password get-value)))
                                                                        (cond
                                                                          [t (send login-frame show #f)
                                                                             (write-history (send server get-value)
                                                                                            (send username get-value))
                                                                             (startup (send username get-value)
                                                                                      (send current-login-context get-group))]
                                                                          [else (message-box "error" "用户名密码错误" login-frame)])
                                                                        )]))
(define cancle (new button%[label "取消"][parent h1][callback(lambda(b e)(send login-frame show #f))]))
(define c (new message%	 
               [parent p]
               [min-width 695][min-height 510]
               ;[style '(transparent)]
               [label (read-bitmap logo)])
              ; [paint-callback (lambda(t dc)(send dc draw-bitmap (read-bitmap logo) 0 0))]
               )
;(define v1 (new vertical-pane% [parent p1]))


(send login-frame end-container-sequence)
;(+ 1 "a")

(send login-frame show #t)
(save-main-config!)

