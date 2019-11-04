#lang racket/base
(require "nav8-base.rkt"
         "private/user.rkt"
         "private/database.rkt"
         "private/main-frame.rkt"
         "tools.rkt"
         racket/class
         racket/gui/base
         db/base)

(provide load-data init!)
(send page1 begin-container-sequence)
(define page1-info (new vertical-panel% [parent page1]))
(define tools (new horizontal-panel%[parent page1-info][stretchable-height #f][enabled #f]))
(define nv81 (new list-box-sort-by-header%
               [parent page1-info]
               [columns '("用户名" "类型" "描述")]))
(define add@ (new button%[label "新增"][parent tools]
                  [callback (lambda(b e)(new-dialog))]
                  ))
(define delete@ (new button%[label "删除"][parent tools]
                     [callback (lambda(b e)
                                 (define cnn (get-cnn))
                                 (define cc (send nv81 get-selections))
                                 (unless (null? cc)
                                   (define name (send nv81 get-data (car cc)))
                                   (delete-user cnn name)
                                   (load-data cnn)))]))
(define edit@ (new button%[label "编辑"][parent tools]
                   [callback (lambda(b e)
                               (define cnn (get-cnn))
                               (define cc (send nv81 get-selections))
                               (unless (null? cc)
                                 (define name (send nv81 get-data (car cc)))
                                 (define type-info (query-row cnn "select pass,igroup,info from aqjcusers where name=$1" name))
                                 (edit-dialog name (vector-ref type-info 0)(vector-ref type-info 1)(vector-ref type-info 2))))]))
(define(type->usertypename t)
  (case t
    [(0)"公共用户"]
    [(1)"普通用户"]
    [else "超级用户"]))

(define(get-user-data cnn)
  (for/list([(a b c)(in-query cnn "select name,igroup,info from aqjcuserinfo")])
    (vector a (type->usertypename b) c)))
    

(define(create-public-user cnn name pass info)
  (create-user cnn name pass)
  (move-to-group cnn name "aqjcpublic")
  (query-exec cnn "insert into aqjcusers(name,igroup,pass,info)values($1,$2,$3,$4)" name 0 pass info))
(define(create-normal-user cnn name pass info)
  (create-user cnn name pass)
  (move-to-group cnn name "aqjcuser")
  (query-exec cnn "insert into aqjcusers(name,igroup,pass,info)values($1,$2,$3,$4)" name 1 pass info))
(define(create-super-user cnn name pass info)
  (create-user cnn name pass)
  (move-to-group cnn name "aqjcsuper")
  (query-exec cnn "insert into aqjcusers(name,igroup,pass,info)values($1,$2,$3,$4)" name 2 pass info))
(define(delete-user cnn name)
  (drop-role cnn name)
  (query-exec cnn "delete from aqjcusers where name=$1" name))
(define(new-dialog)
  (define cnn (get-cnn))
  (define t(new dialog%
                [label  "创建新用户"]
                [parent main-frame]
                [width   500]
                [stretchable-width  #f]
                [stretchable-height #f]
                ))
  (define p (new vertical-pane%[parent t]))
  (define username(new text-field% [parent p][label "用户名"][min-width 200]))
  (define password(new text-field% [parent p][label "密   码"][style '(single password)]))
  (define type (new choice%[label "类   型 "][parent p][choices '("公共用户" "普通用户" "超级用户")][stretchable-width #t]))
  (define info(new text-field% [parent p][label "描   述"]))
  (define h (new horizontal-pane%[parent t][alignment '(center center)]))
  (define yes (new button%[label "确定"][parent h]
                   [callback (lambda(b e)
                               (define namestr (send username get-value))
                               (define passwordstr (send password get-value))
                               (define infostr (send info get-value))
                               (case (send type get-selection)
                                 [(0)(create-public-user cnn namestr passwordstr infostr)]
                                 [(1)(create-normal-user cnn namestr passwordstr infostr)]
                                 [else(create-super-user cnn namestr passwordstr infostr)])
                               (load-data cnn)
                                (send t show #f))]))
  (define cancel (new button%[label "取消"][parent h]
                      [callback (lambda(b e)(send t show #f))]
                      ))
  (send t show #t))

(define(type->group t)
  (case t
    ([0]"aqjcpublic")
    ([1]"aqjcuser")
    (else "aqjcsuper")))

(define(edit-dialog name pass type_v info_v)
  (define cnn (get-cnn))
  (define t(new dialog%
                [label  "编辑已有用户"]
                [parent main-frame]
                [width   500]
                [stretchable-width  #f]
                [stretchable-height #f]
                ))
  (define p (new vertical-pane%[parent t]))
  (define username(new text-field%
                       [parent p]
                       [label "用户名"]
                       [init-value name]
                       [enabled #f]
                       [min-width 200]))
  (define password(new text-field% [parent p][label "密   码"]
                       [style '(single password)][init-value pass]))
  (define type (new choice%[label "类   型 "][parent p][choices '("公共用户" "普通用户" "超级用户")]
                    [selection type_v]
                    [stretchable-width #t]))
  (define info(new text-field% [parent p][label "描   述"][init-value info_v]))
  (define h (new horizontal-pane%[parent t][alignment '(center center)]))
  (define yes (new button%[label "确定"][parent h]
                   [callback (lambda(b e)
                               (remove-from-group cnn name (type->group type_v))
                               (delete-user cnn name)
                               (define namestr (send username get-value))
                               (define passwordstr (send password get-value))
                               (define infostr (send info get-value))
                               (case (send type get-selection)
                                 [(0)(create-public-user cnn namestr passwordstr infostr)]
                                 [(1)(create-normal-user cnn namestr passwordstr infostr)]
                                 [else(create-super-user cnn namestr passwordstr infostr)])
                               (load-data cnn)
                               (send t show #f))]))
  (define cancel (new button%[label "取消"][parent h]
                      [callback (lambda(b e)(send t show #f))]
                      ))
  (send t show #t))

(define(load-data cnn)
  (send nv81 load-data (get-user-data cnn)))

(send page1 end-container-sequence)

(define(init!)
  ;(displayln "init")
  (when (is-owner?)
   ; (displayln "init...")
    (send tools enable #t)))
