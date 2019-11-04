#lang racket/base
(require ;"private/database.rkt"
        ; "private/preferences.rkt"
         "private/main-frame.rkt"
         "private/list-box.rkt"
         "private/pdf-render.rkt"
         (prefix-in t:"private/table-cell-class.rkt")
         (prefix-in word: "private/table-cell-class.rkt")
         "private/date-panel.rkt"
         "private/writing-word.rkt"
         "nav6-base.rkt"
         "private/word_to_pdf.rkt"
         "private/draw.rkt"
         ;"private/table.rkt"
         "tools.rkt"
         table-panel
         racket/gui/base
         racket/class
        ; racket/file
       ;  racket/port
         racket/path
         racket/set
         racket/runtime-path
         db/base
         json
         framework
         
         )
(provide editer-box init! viewer-file-list)
(send main-frame begin-container-sequence)

(define file-list-and-viewer(new panel:horizontal-dragable%  [parent page2][min-height 500][stretchable-height #t]))
(define file-panel(new panel%[parent file-list-and-viewer]))
(define file-list-panel (new vertical-panel% [parent file-panel][min-width 495]))
(define nv-tools(new horizontal-panel%[parent file-list-panel][stretchable-height #f]))

(define(hide-file-viewer)
  ;(send file-list-and-viewer show #f)
  (send file-list-panel reparent  page2)
  (send page2 active-child file-list-panel)
  )
(define(show-file-viewer)
  (send file-list-panel reparent  file-panel)
  (send page2 active-child file-list-and-viewer)
  ;(send file-list-and-viewer show #t)
  )
(define viewer-file-list
  (new list-box-sort-able%
       [parent file-list-panel]
      ; [multi-choices '()];(query-rows (get-cnn) "select filename,date from files where subindex=$1 order by sortindex" (current-subindex))
       [on-dclick (lambda(x)
                    (define t (make-temp-pdf-file))
                    (write-out-file (get-cnn) (vector-ref x 0)t)
                    ;(show-file-viewer)
                    (show-view)
                    (send file-viewer load-file* t)
                    )]
       [on-click (lambda(x);(printf "~a\n" x)
                   (cond
                     [(null? x)(send delete-button enable #f)
                               (send up-button enable #f)
                               (send down-button enable #f)
                               (send edit-button enable #f)
                               ]
                     [(null? (cdr x))(enable-delete)
                                     (enable-edit)
                                     (enable-up)
                                     (enable-down)
                                     
                                     ]
                     [else (enable-delete)
                           (send edit-button enable #f)
                           (send up-button enable #f)
                           (send down-button enable #f)])
                   )]
       [reload (lambda(x)(send viewer-file-list
                               load-multi-choices(query-subindex (get-cnn) x)))]
       [columns '("文件名" "日期")]
       [columns-types '("text" "date")]))
(define view-or-edit(new (panel:single-mixin panel%)[parent file-list-and-viewer]))
(send file-list-and-viewer set-percentages '(1/3 2/3))
(define file-viewer (new pdf-viewer-panel%
                    [parent view-or-edit]
                    [style '(border)]))
(define editer (new panel%[parent view-or-edit]))
(define editer-box (new vertical-panel%
                    [parent editer]
                   ; [style '(border)]
                    [style '(auto-vscroll border)]
                    ))

(define(show-editer*)
  (send view-or-edit active-child editer))
(define(show-file-viewer*)
  (send view-or-edit active-child file-viewer))

(define up-button (new button% [parent nv-tools][label "上移"][enabled #f]
                       [callback (lambda(b e)(send viewer-file-list move-up-select))]))
(define down-button (new button% [parent nv-tools][label "下移"][enabled #f]
                         [callback (lambda(b e)(send viewer-file-list move-down-select))]))
(define delete-button (new button% [parent nv-tools][label "删除"][enabled #f]
                            [callback (lambda(b e)
                                        (define sels(send viewer-file-list get-selection-files-id))
                                       ; (displayln sels)
                                        (define t (message-box "警告" "确定要删除?" #f '(caution yes-no)))
                                        (when (eq? t 'yes)
                                        (call-with-transaction (get-cnn)
                                                               (lambda()
                                                                 (for([i (in-list sels)])
                                                                  ; (displayln i)
                                                                    (files:delete (get-cnn)  i))))
                                        (send viewer-file-list reload* (get-current-subindex))))]))
#|
(define insert-button (new button% [parent nv-tools][label "导入"]
                           [callback (lambda( b e)
                                       (define f(get-file #f #f #f #f #f '() '(("pdf file" "*.pdf")("word file" "*.docx" ))))
                                       (when f
                                         (files:move-up-all (get-cnn) (get-current-subindex))
                                         (define id (files:insert* (get-cnn) (get-current-subindex) f))
                                         (send viewer-file-list insert-line
                                               
                                               0
                                               (vector id (path->string(file-name-from-path f)) (current-sql-date))
                                               )
                                         ))]))
|#
(define(reset-enable)
  (send delete-button enable #f)
  (send up-button enable #f)
  (send down-button enable #f))



(define current-edit-mode (box 'none));edit new
(define current-edit-state (box 'none));
(define current-edit-key (box -1))
(define(get-current-edit-key)
  (unbox current-edit-key))
(define(get-current-edit-mode)
  (unbox current-edit-mode))
(define state (box 'view))
(define(get-state)
  (unbox state))



(define(show-list)
  (unless (set-member?  '(lst-edit lst-view) (get-state))
    ;(send file-list-and-viewer show #f)
    (hide-file-viewer)
    
    (case (get-state)
      [(view)(set-box! state 'lst-view)]
      [(edit)(set-box! state 'lst-edit)]
      [else (error 'show-list "unknow state ~a" (get-state))])))
(define(show-view)
  (unless (eq? (get-state) 'view)
    (show-file-viewer)
    (case (get-state)
      [(lst-view)(show-file-viewer*)]
      [(lst-edit)(show-file-viewer*)]
      [(edit)(show-file-viewer*)]
      [else (error 'show-view "unknow state ~a" (get-state))])
    ;(send nv-side enable #f)
    (set-box! state 'view)
    (set-box! current-edit-mode 'none)
    (set-box! current-edit-state 'none)
    (set-box! current-edit-key -1)
    ))
(define(show-edit)
  (unless (eq? (get-state) 'edit)
    (show-file-viewer)
    (case (get-state)
      [(lst-view)(show-editer*)]
      [(lst-edit)(show-editer*)]
      [(view)(show-editer*)]
      [else (error 'show-view "unknow state ~a" (get-state))])
    ;(send nv-side enable #t)
    (set-box! state 'edit)))

;-------------------------------------------------------------------------------------------------------------
(define table (new vertical-panel% [parent editer-box]))
(void (new message%[label "航空机务质量安全问题免责报告信息表"][parent table]))
(define t (new horizontal-panel%[parent table][spacing 300][stretchable-height #f]))
(void (new message%[label "填表日期"][parent t]	[horiz-margin 20]))
(define edit-date (new date-choicer% [parent t][label #f][horiz-margin 20]))
(define info (new vertical-panel% [parent table][style '(border)][stretchable-height #f]))
(define msg (new panel% [parent info][style '(border)][stretchable-height #f]))
(void (new message%[label
                    "      您在航空维修保障中出现的任何危险性故障、差错和遇到的不安全事件,别人也
可能遇到,为了战友的生命安全和国家的财产安全,请您主动、如实的报告您所经历的
、看到的各种不安全隐患。您的报告绝不带个人信息进行公开,也不会作为任何检查
、评比和处罚的依据。"][parent msg][stretchable-height #f]	[auto-resize #t]))
(define tb (new table-panel%[parent info][dimensions  '(4 2)]))
;(define t2 (new horizontal-panel%[parent info][style '(border)][stretchable-height #f]))
(define msg2 (new panel% [parent tb][style '(border)]))
(define t3 (new vertical-panel% [parent tb][style '(border)]))

(void (new message%[label
                    "事\n件\n基\n本\n情\n况"][parent msg2][auto-resize #t]))
(define hx (new horizontal-panel%[parent t3][stretchable-height #f]))
(define unit (new text-field% [label "单位: 空"][parent hx][stretchable-width #f][min-width 20]))
(define unit2 (new text-field% [label "团"][parent hx]))
(define unit3 (new text-field% [label "关键词(问题简述)"][parent hx]))
(define time (new date-choicer% [parent t3][label "时间:"]))
(define where (new text-field% [label "地点:"][parent t3]))
(define why (new text-field% [label "时机:"][parent t3]))
(define role (new text-field% [label "您的岗位角色:"][parent t3]))
(define airplane-type (new text-field% [label "机型:"][parent t3]))
(define t4 (new horizontal-panel%[parent t3][stretchable-height #f]))
(define name (new text-field% [label "报告人署名(完全自愿):"][parent t4]))
(define phone (new text-field% [label "电话(完全自愿):"][parent t4]))
(define msg3 (new panel% [parent tb][style '(border)]))
(void (new message%[label
                    "事\n件\n经\n过"][parent msg3][stretchable-height #f][auto-resize #t]))

(define text1 (new panel% [parent tb][style '(border)]))

(define process (new text-field%[label #f][parent text1][style '(multiple)]))
(define msg4 (new panel% [parent tb][style '(border)]))
(void (new message%[label
                    "事件分析\n及\n改进建议"][parent msg4][stretchable-height #f][auto-resize #t]))
(define text2 (new vertical-panel% [parent tb][style '(border)]))
(define analysis (new text-field%[label "事件分析:"][parent text2][style '(multiple)]))
(define advise (new text-field%[label "改进建议:"][parent text2][style '(multiple)]))
(define msg5 (new panel% [parent tb][style '(border)]))
(void (new message%[label
                    "原因分类\n(必填项,请\n在□内打√,\n16项只选\n一项)"][parent msg5][stretchable-height #f][auto-resize #t]))
(define ts (new vertical-panel% [parent tb][style '(border)]))
(define ts1(new horizontal-panel% [parent ts]))
(define ts2(new horizontal-panel% [parent ts]))
(define ts3(new horizontal-panel% [parent ts]))
(define ts4(new horizontal-panel% [parent ts]))
(void (new message% [label "不安全行为:"][parent ts1]))
(define check-box-control%
  (class object%
    (super-new)
    (init-field[items '()])
    (define m "不安全行为:□技能差错□决策差错□知觉差错□习惯性违规□偶然性违规
不安全行为的条件:□作业环境□技术环境□操作者状态□操作者表现
不安全的管理:□不能识别危险□问题没有纠正□管理不到位□管理冲突
组织影响:□资源管理□组织氛围□组织程序")
    (define(gen-out-str i)
      (regexp-replace (pregexp (format"^.*?(□.*?){~a}(?:□)" i)) m
                   (lambda(all ex)
                     (regexp-replace #rx"□$" all "√"))))
    (define/public(get-value-string)
      (define t(for/first([i (in-naturals 0)]
                 [j (in-list items)]
                 #:when (send j get-value))
            i))
      (if t
         (gen-out-str t)
         m))
    (define/public(get-value)
     (for/first([i (in-naturals 0)]
                 [j (in-list items)]
                 #:when (send j get-value))
            i))
    (define/public(set-value v)
      (for([i (in-list items)])
        (send i set-value #f))
      (when v
       (send (list-ref items v) set-value #t)))
    (define/public(item-check item)
      (for([i (in-list items)]
           #:unless (object=? i item))
        (send i set-value #f)))))
(define ts11 (new check-box%  [label "技能差错"][parent ts1][callback (lambda(b e)(send cc item-check b))][value #t]))
(define ts12 (new check-box%  [label "决策差错"][parent ts1][callback (lambda(b e)(send cc item-check b))]))
(define ts13 (new check-box%  [label "知觉差错"][parent ts1][callback (lambda(b e)(send cc item-check b))]))
(define ts14 (new check-box%  [label "习惯性违规"][parent ts1][callback (lambda(b e)(send cc item-check b))]))
(define ts15 (new check-box%  [label "偶然性违规"][parent ts1][callback (lambda(b e)(send cc item-check b))]))
(void (new message% [label "不安全行为的条件:"][parent ts2]))
(define ts21 (new check-box%  [label "作业环境"][parent ts2][callback (lambda(b e)(send cc item-check b))]))
(define ts22 (new check-box%  [label "技术环境"][parent ts2][callback (lambda(b e)(send cc item-check b))]))
(define ts23 (new check-box%  [label "操作者状态"][parent ts2][callback (lambda(b e)(send cc item-check b))]))
(define ts24 (new check-box%  [label "操作者表现"][parent ts2][callback (lambda(b e)(send cc item-check b))]))
(void (new message% [label "不安全的管理:"][parent ts3]))
(define ts31 (new check-box%  [label "不能识别的危险"][parent ts3][callback (lambda(b e)(send cc item-check b))]))
(define ts32 (new check-box%  [label "问题没有纠正"][parent ts3][callback (lambda(b e)(send cc item-check b))]))
(define ts33 (new check-box%  [label "管理不到位"][parent ts3][callback (lambda(b e)(send cc item-check b))]))
(define ts34 (new check-box%  [label "管理冲突"][parent ts3][callback (lambda(b e)(send cc item-check b))]))
(void (new message% [label "组织影响:"][parent ts4]))
(define ts41 (new check-box%  [label "资源管理"][parent ts4][callback (lambda(b e)(send cc item-check b))]))
(define ts42 (new check-box%  [label "组织氛围"][parent ts4][callback (lambda(b e)(send cc item-check b))]))
(define ts43 (new check-box%  [label "组织程序"][parent ts4][callback (lambda(b e)(send cc item-check b))]))
(define cc (new check-box-control%
                [items (list ts11 ts12 ts13 ts14 ts15 ts21 ts22 ts23 ts24 ts31 ts32 ts33 ts34 ts41 ts42 ts43)]))
;(define ts34 (new check-box%  [label "管理冲突"][parent ts3]))


(define-runtime-path model-path "private/model/mzbg.docx")

(define(load-metadata lst)
  (for([i (in-list lst)]
       [j (in-list (list edit-date unit unit2 time where
                         why role airplane-type name phone
                         process analysis advise cc))])
    (send j set-value i)))
(define(get-metadata)
(jsexpr->bytes	(for/list([j (in-list (list edit-date unit unit2 time where
                         why role airplane-type name phone
                         process analysis advise cc))])
    (send j get-value))))
(define(save-this path)
  (define word (open-word-auto))
  (define doc(open-doc word model-path))
  (define tbs (file-get-tables doc))
  (define tb1 (item-ref tbs 1))
  (define(wb i j txt)
    (cell-insert-text! (table-ref tb1 i j) txt))
  (define(wu i j u )
    (cell-insert-text! (table-ref tb1 i j) (send u get-value)))
  (wu 1 2 edit-date)
  (wu 3 3 unit)
  (wu 3 5 unit2)
  (wu 4 3 time)
  (wu 5 3 where)
  (wu 6 3 why)
  (wu 7 3 role)
  (wu 8 3 airplane-type)
  (wu 9 3 name)
  (wu 9 5 phone)
  (wu 10 2 process)
  (wu 11 3 analysis)
  (wu 12 3 advise)
  (wb 13 2 (send cc get-value-string))
  (word-to-pdf-file doc path)
  (close-file-without-save-change doc))

(define hsss (new horizontal-panel% [parent info][alignment '(center center)][spacing 50]))
(define save-button (new button% [label "保存"][parent hsss]
                           [callback (lambda(b e)
                               (define m model-path) 
                               ;(printf "model ~a\n" m)
                               (cond
                                 [(file-exists? m);(send message-side set-label "正在处理...")
                                                    (send gauge set-value 3)
                                                    (send  gauge refresh)
                                                    ;(sleep 0.1)
                                                    (send gauge show #t)
                                                    ;(send  nv-tools refresh)
                                                    ;(define word (open-word-auto))
                                                    ;(send gauge set-value 4)
                                                  ;(define doc (com-get-property word "Documents"))
                                                    ;(define file (open-doc word  (path->string m)))
                                                    
                                                
                                                  
                                                  (define out (make-temp-pdf-file))
                                                  (save-this (path->string out))
                                                  (send gauge set-value 4)
                                                  ;(word-to-pdf-file file out)
                                                   ; (send gauge set-value 4)
                                                  ;
                                                    ;(send gauge set-value 5)
                                                  (define mode (get-current-edit-mode))
                                                  (case mode
                                                    [(new)(define id(mzbg-insert  (get-cnn)
                                                                                  (get-current-subindex)
                                                                                  out
                                                                                  (send unit3 get-value)
                                                                                  (get-metadata)
                                                                                  (send cc get-value)
                                                                                  (send time get-value)))
                                                          (send viewer-file-list reload* (get-current-subindex))]
                                                    [(edit)(mzbg-update (get-cnn)
                                                                                  (get-current-edit-key)
                                                                               out
                                                                               (path->string(file-name-from-path out))
                                                                               (get-metadata)
                                                                               (send cc get-value)
                                                                                  (send time get-value))]
                                                     [else(error 'save "error when save data mode can not be ~a\n" mode)])
                                                     (send gauge set-value 6)
                                                  
                                                    (send gauge set-value 7)
                                                    (send new-button enable #t)
                                                    ;(send message-side set-label "保存完毕")
                                                    (set-box! current-edit-state 'success);
                                                    (send gauge set-value 0)
                                                    (send gauge show #f)]))]))
(define cancle-button (new button% [label "取消"][parent hsss]
                           [callback (lambda(b e)
                                              (define state(case (unbox current-edit-state)
                                                         [(none success)'yes]
                                                         [else(message-box "提示" "您正在进行的编辑尚未保存!\n是否放弃更改?" main-frame
                                                                            '(yes-no no-icon))]))
                                              (when (eq? state 'yes)
                                                (show-list)
                                                (send new-button enable #t)))]))
(define edit-button (new button% [parent nv-tools][label "编辑"][enabled #f]
                        [callback (lambda(b e)
                                        (define mode (get-current-edit-mode))
                                        (define sels(send viewer-file-list get-selection-files-id))
                                        (when (= 1 (length sels))
                                          (define choice-this (car sels))
                                          ;(displayln mode)
                                          (define state(case mode
                                                         [(none)'yes]
                                                         [(edit)(if (=(get-current-edit-key) choice-this) 'nothing
                                                                    (message-box "提示" "您正在进行的编辑尚未保存!\n是否放弃更改?" main-frame
                                                                                 '(yes-no no-icon)))]
                                                         [(new)(message-box "提示" "您正在进行的编辑尚未保存!\n是否放弃更改?" main-frame
                                                                            '(yes-no no-icon))]))
                                          ;(displayln mode)
                                          (when (eq? state 'yes)
                                            ;(send message-side set-label "     编辑")
                                            (show-edit)
                                            (send new-button enable #f)
                                            (set-box! current-edit-key choice-this)
                                            (set-box! current-edit-mode 'edit)
                                            (set-box! current-edit-state 'start)
                                            (load-metadata (bytes->jsexpr (files_editable:get-metadata (get-cnn) choice-this))))
                                        ))]))
(define new-button (new button% [parent nv-tools][label "新建"][enabled #f]
                        [callback (lambda(b e)
                                        (show-edit)
                                        (set-box! current-edit-key -1)
                                       ; (send message-side set-label "     新建")
                                        ;(send task-name enable #t)
                                        ;(send new-button enable #f)
                                        ;(send edit-button enable #f)
                                    (set-box! current-edit-mode 'new)
                                    (set-box! current-edit-state 'start)
                                        )]))
(define collect-button (new button% [parent nv-tools][label "统计"]
                        [callback (lambda(b e)
                                    (start-collect-window)
                                        )]))


(define(start-collect-window)
  (define m (new frame%[label "统计分析"]
                 [width 1100]
                 [height 500]
                 [parent main-frame]))
  (define v (new vertical-panel% [parent m][spacing  10]))
  (define tools (new horizontal-panel%[parent v][stretchable-height #f]))
  (define begin-date (new date-choicer% [parent tools][label "时间"]))
  (define end-date (new date-choicer% [parent tools][label "-"]))
  (define draw-button (new button%[parent tools][label "查询"]
                           [callback(lambda(b e)(draw-this*))]))
  (define(draw-this*)
  (draw-pict (mzbg-select-type (get-cnn) (send begin-date get-value) (send end-date get-value))))
(define(draw-pict lst)
  (define bm(draw-nv6* 1000 400 lst))
  (send p1 load-bitmap bm))
  (define pbox (new panel%[parent v][stretchable-height #f]
                    [stretchable-width #f][min-width 1000][min-height 400]))
(define p1 (new t:pict% [parent pbox]))
  (send m show #t))
(define gauge (new gauge%
                   (label "进度  ")
                   (parent nv-tools)
                   (range 7)
                   (min-width 100)
                   (stretchable-width #f)))
(send main-frame end-container-sequence)

(define(init!)
 ; (printf "init...~a\n" (get-group))
  (enable-new))


(define(enable-up)
  (when (>(get-group)1)
    (send up-button enable #t)))
(define(enable-down)
  (when (>(get-group)1)
    (send down-button enable #t)))
(define(enable-delete)
  (when (>(get-group)1)
    (send delete-button enable #t)))
(define(enable-edit)
  (when (>(get-group)0)
    (send edit-button enable #t)))
(define(enable-new)
  (when (>(get-group)0)
    (send new-button enable #t)))
(send gauge show #f)
