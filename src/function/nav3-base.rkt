#lang racket/base
(require "private/main-frame.rkt"
         "private/list-box.rkt"
        "private/active-unit.rkt"
         "private/pdf-render.rkt"
         "private/panel.rkt"
         "private/table-cell-mixin.rkt"
          "private/writing-word.rkt"
         "private/word_to_pdf.rkt"
         "tools.rkt"
         "private/table.rkt"
         "private/scrollbar.rkt"
         racket/runtime-path
         db/base
         racket/set
         racket/path
         racket/gui/base
         racket/class
         framework
         (prefix-in t: "private/table-cell-class.rkt"))
(provide nv
         nv-side
         nv-info
         nv-tools
         file-list-and-viewer
         view-or-edit
         viewer-file-list
         file-viewer
         editer
         scroll@x scroll@y
         page1
         page2
         hide-page2
         show-page2
         editer-tools
         reload-nv3
         show-list
         show-edit
         p1 p2 p3 p4 p5 p6
         init!
         )
(send main-frame begin-container-sequence)
(define nv(new horizontal-panel%[parent view-panel]))
(define(reload-nv3)
  (send viewer-file-list reload* (get-current-subindex)))
(define nv-side (new (active-panel-mixin% vertical-panel%) [parent nv]
                        [style '( border)]
                        [enabled #f]
                       ; [min-width side-panel-min-width]
                       ; [min-height side-panel-min-height]
                        [stretchable-width #f]))
(define nv-info (new (panel:single-mixin panel%)
                       ; [label "info"]
                        [parent nv]
                        [style '(border)]
                        ;[min-width info-min-width]
                        ;[min-height info-min-height]
                        ))

(define file-list-and-viewer(new panel:horizontal-dragable%  [parent nv-info][stretchable-height #t][style '(border)]))
(define file-panel(new panel%[parent file-list-and-viewer]))
(define file-list-panel (new vertical-panel% [parent file-panel][min-width 495]))
(define nv-tools(new horizontal-panel%[parent file-list-panel][stretchable-height #f][enabled #f]))
;----------------------------------------
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
;-------------------------------------------------
(define viewer-file-list
  (new list-box-sort-able%
       [parent file-list-panel]
      ; [multi-choices '()];(query-rows (get-cnn) "select filename,date from files where subindex=$1 order by sortindex" (current-subindex))
       [on-dclick (lambda(x)
                    
                    (define yes?(if(eq?(get-state) 'edit)
                         (message-box "提示" "您正在进行的编辑尚未保存!\n是否放弃更改?" main-frame
                                           '(yes-no no-icon))
                         'yes))
                    
                    (when (eq? yes? 'yes)
                      (define t (make-temp-pdf-file))
                    (write-out-file (get-cnn) (vector-ref x 0)t )
                    (show-view)
                    (send file-viewer load-file* t)
                      )
                    ;(show-view)
                    )]
       [on-click (lambda(x);(printf "~a\n" x)
                   (cond
                     [(null? x)(send delete-button enable #f)
                               (send up-button enable #f)
                               (send down-button enable #f)
                               (send edit-button enable #f)]
                     [(null? (cdr x))(enable-delete)
                                     (enable-edit)
                                     (enable-up)
                                     (enable-down)
                                     ]
                     [else (enable-delete)
                           (send edit-button enable #f)
                           (send up-button enable #f)
                           (send down-button enable #f)
                           ])
                   )]
       [reload (lambda(x)(send viewer-file-list
                               load-multi-choices(query-subindex (get-cnn) x)))]
       [columns '("文件名" "日期")]
       [columns-types '("text" "date")]))



(define view-or-edit(new (panel:single-mixin panel%)[parent file-list-and-viewer]))
(define file-viewer (new pdf-viewer-panel%
                    [parent view-or-edit]
                     [style '(border )]
                    ))

(define editer (new panel%[parent view-or-edit]))
(define editer-box (new vertical-panel%
                    [parent editer]
                   ; [style '(border)]
                    [style '(auto-vscroll border)]
                    ))

(define page1 (new horizontal-merge-table-panel%
       [columns 6]
       [rows 16]
       [parent editer-box]
      ; [style '(border)]
       ))
(define page2 (new horizontal-merge-table-panel%
       [columns 6]
       [rows 13]
       [parent editer-box]
       ;[style '(border)]
       ))
(define editer-tools (new horizontal-panel%[parent editer-box][alignment '(center center)]))

(define(hide-page2)
  (send editer-box change-children (lambda(x)(list page1 editer-tools))))
(hide-page2)
(define(show-page2)
  (send editer-box change-children (lambda(x)(list page1 page2 editer-tools)))
  (when (=(get-group)1)
     (send page1 enable #f))
  )

(define(show-list)
  (unless (set-member?  '(lst-edit lst-view) (get-state))
    ;(send file-list-and-viewer show #f)
    (send file-list-panel reparent nv-info)
    (send nv-info active-child file-list-panel)
    (case (get-state)
      [(view)(set-box! state 'lst-view)]
      [(edit)(set-box! state 'lst-edit)]
      [else (error 'show-list "unknow state ~a" (get-state))])
    (send nv-side enable #f)))
(define(show-view)
  (unless (eq? (get-state) 'view)
    (case (get-state)
      [(lst-view);(displayln 1)
                 (send file-list-panel reparent  file-panel)
                 (send file-list-and-viewer show #t)]
      [(lst-edit);(displayln 2)
                 (send file-list-panel reparent  file-panel)
                 (send view-or-edit active-child  file-viewer)
                 (send file-list-and-viewer show #t)]
      [(edit)(send view-or-edit active-child  file-viewer)]
      [else (error 'show-view "unknow state ~a" (get-state))])
    (send nv-side enable #f)
    (set-box! state 'view)
    (set-box! current-edit-mode 'none)
    (set-box! current-edit-state 'none)
    (set-box! current-edit-key -1)
    ))
(define(show-edit)
  (unless (eq? (get-state) 'edit)
    (case (get-state)
      [(lst-view)(send file-list-panel reparent  file-panel)
                 (send file-list-and-viewer show #t)
                 (send view-or-edit active-child  editer)]
      [(lst-edit)(send file-list-panel reparent  file-panel)
                 (send file-list-and-viewer show #t)]
      [(view)(send view-or-edit active-child  editer)]
      [else (error 'show-view "unknow state ~a" (get-state))])
    (send nv-side enable #t)
    (set-box! state 'edit)))
;----------------------------

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
(send page1 begin-container-sequence)
(define word:constant% (word-table-cell-mixin t:constant%))
(define word:text% (word-table-cell-mixin t:text%))
(define word:date% (word-table-cell-mixin t:date%))
(define word:choice% (word-table-cell-mixin t:choice%))
(define word:time% (word-table-cell-mixin t:time%))
(define word:pict% (word-table-cell-mixin t:pict%))
(define word:button% (word-table-cell-mixin t:button%))
(define word:yes/no% (word-table-cell-mixin t:yes/no%))

(define(make-const pos merge label )
  (new word:constant%  [label label][pos pos][merge merge]
  [parent page1]))
(define(make-text pos merge  #:init [init ""])
  (new word:text% [pos pos][merge merge][parent page1][init-value init]))

(void (make-const (pos 1 1)(colmerge 1 6) "监察实施"))
(define p1 (make-const (pos 2 1)(colmerge 1 6) "领受任务"))

(void(make-const (pos 3 1)(colmerge 1 1) "日期"))
(void(new word:date%
          [vert-margin 0]
          [min-width 50]
          [pos (pos 3 2)]
          [horiz-margin 0]
          [merge (colmerge 2 2)]
          [parent page1]))
(void (make-const (pos 3 3)(colmerge 3 3) "任务名称"))
(define task-name (make-text (pos 3 4)(colmerge 4 6)))
(define p2 (make-const (pos 4 1)(colmerge 1 6) "监察准备"))
(void (make-const (pos 5 1) (colmerge 1 1) "监察方式"))
(void (new word:choice% [choices '("日常监察" "定期监察" "重点监察" "专项监察")]
               [pos (pos 5 2)][merge (colmerge 2 3)][parent page1]))
(void (make-const (pos 5 3)(colmerge 4 4) "监察方法"))
(void (new word:choice% [parent  page1][pos (pos 5 4)][merge (colmerge 5 6)]
           [choices '("现场巡查" "随机抽查""跟班检查""录音录像""参加会议""座谈讨论""个别交流""查看监控""查阅登统计""组织考试")]))
(void (make-const (pos 6 1) (colmerge 1 1) "监察单号"))
(void (make-text (pos 6 2) (colmerge 2 3)))
(void (make-const (pos 6 3)(colmerge 4 4) "法规依据"))
(void (make-text (pos 6 4)(colmerge 5 6)))
(void (make-const (pos 7 1)(colmerge 1 1) "注意事项"))
(void (new word:text% [pos (pos 7 2)][merge (colmerge 2 6)][parent page1]
           [style '(multiple)]))
(define p3  (make-const (pos 8 1)(colmerge 1 6) "监察开展"))
(void (make-const (pos 9 1)(colmerge 1 1) "时间"))
(void (new word:time% [parent page1][pos (pos 9 2)][merge (colmerge 2 2)][min-width 90]))
(void (make-const (pos 9 3)(colmerge 3 3) "监察人员"))
(void (new word:choice% [parent  page1][pos (pos 9 4)][merge (colmerge 4 4)]
           [choices '("001" "002" "003" "004" "005")]))
(void (make-const (pos 9 5)(colmerge 5 5) "监察地点"))
(void (make-text (pos 9 6)(colmerge 6 6)))
(define p4 (make-const (pos 10 1)(colmerge 1 6) "问题查处"))
(define t (make-const (pos 11 1)(colmerge 1 1) "问题编号"))
(void (make-const (pos 11 2)(colmerge 2 6) "问题内容"))
(define t2 (make-const (pos 13 1)(colmerge 1 6) "取证记录"))
(define p5 t2)
(define m1 (new t:table-variable-row-manger%
           [init-pos t]
           [parent page1]
           [once-repeat-times 3]
           [once-create-rows 3]
           [index 0]
           [repeat-proc (lambda(count  init-row once-repeat-times)
                               (apply append (for/list([i (in-range init-row (+ init-row once-repeat-times))]
                                              [j (in-naturals (add1 count))])
                                           (list (make-const (pos i 1)(colmerge 1 1) (format "~a" j))
                                                   (make-text (pos i 2)(colmerge 2 6))))))]))
(define m2 (new t:table-variable-row-manger%
                [repeat-proc (lambda(count  init-row once-repeat-times)
                               (apply append (for/list([i (in-range init-row (+ init-row 1))])
                                 (list (new word:pict%[parent page1][pos (pos i 1)][merge (colmerge 1 2)][min-height 200][horiz-margin 10])
                                       (new word:pict%[parent page1][pos (pos i 2)][merge (colmerge 3 4)][min-height 200][horiz-margin 10])
                                       (new word:pict%[parent page1][pos (pos i 3)][merge (colmerge 5 6)][min-height 200][horiz-margin 10])
                                       (make-text (pos (add1 i) 1)(colmerge 1 2)#:init (format "~a.问题描述" (+ count 1)))
                                       (make-text (pos (add1 i) 2)(colmerge 3 4)#:init (format "~a.问题描述"(+ count 2)))
                                       (make-text (pos (add1 i) 3)(colmerge 5 6)#:init (format "~a.问题描述" (+ count 3)))))))]
                [init-pos t2]
                [index 1]
                [once-create-rows 2]
                [parent page1]
                [once-repeat-times 3]))

(send page1 set-virtual-line! 12)
(void (new word:button% [parent page1][pos (pos 12 1)][merge (colmerge 2 3)][label "+"][callback (lambda(b e)(send m2 add)
                                                                                    (send m1 add)
                                                                                              )]))
(void (new word:button% [parent page1][pos (pos 12 2)][merge (colmerge 4 5)][label "-"][callback (lambda(b e)(send m2 sub)
                                                                                              (send m1 sub))]))
; (send m2 add)(send m1 add)
(define p6 (make-const (pos 14 1)(colmerge 1 6) "督促整改"))
(define t3 (make-const (pos 15 1)(colmerge 1 1) "问题编号"))
(void (make-const (pos 15 2)(colmerge 2 5) "问题描述"))
(void(make-const (pos 15 3)(colmerge 6 6) "下发整改通知单"))
(define m3 (new t:table-variable-row-manger%
           [init-pos t3]
           [parent page1]
           [once-repeat-times 3]
           [once-create-rows 3]
           [index 2]
           [repeat-proc (lambda(count  init-row once-repeat-times)
                               (apply append (for/list([i (in-range init-row (+ init-row once-repeat-times))]
                                              [j (in-naturals (add1 count))])
                                           (list  (make-const (pos i 1)(colmerge 1 1) (format "~a" j))
                                                   (make-text (pos i 2)(colmerge 2 5))
                                                   (new word:yes/no%
                                                        [parent page1]
                                                        [pos (pos i 3)]
                                                        [merge (colmerge 6 6)]
                                                        [callback (lambda(b e)
                                                                    (define ls(send m3 get-lines))
                                                                    (define show?(for*/or ([i (in-list ls)]
                                                                              [j (in-range 2 10 3)])
                                                                           (send (list-ref i j) select-yes?)))
                                                                    (if show? (show-page2)(hide-page2)))]
                                                        )))))]))
(define(show-page2*)
 (define ls(send m3 get-lines))
  (define show?(for*/or ([i (in-list ls)]
                         [j (in-range 2 10 3)])
                 (send (list-ref i j) select-yes?)))
  (if show? (show-page2)(hide-page2)))
(void (new word:button% [parent page1][pos (pos 16 1)][merge (colmerge 2 3)][label "+"][callback (lambda(b e)(send m3 add)
                                                                                              )]))
(void (new word:button% [parent page1][pos (pos 16 2)][merge (colmerge 4 5)][label "-"][callback (lambda(b e)(send m3 sub))]))
(send m3 add)(send m2 add)(send m1 add)

(define-runtime-path model-path "private/model")
(define(model-choice a b)
  (if (send page2 is-shown?)
  (build-path model-path (format "v~a-~as.docx" a b))
  (build-path model-path (format "v~a-~a.docx" a b))))
(define save-button (new button%[parent editer-tools][label "保存"][vert-margin 20]	 
   	 	[horiz-margin 100]
                   [callback (lambda(b e)
                               (define m (model-choice (send m1 get-count) (send m3 get-count)))
                               ;(printf "model ~a\n" m)
                               (cond
                                 [(file-exists? m);(send message-side set-label "正在处理...")
                                                    (send gauge set-value 3)
                                                    (send  gauge refresh)
                                                    ;(sleep 0.1)
                                                    (send gauge show #t)
                                                    (send  nv-tools refresh)
                                                    (define word (open-word-auto))
                                                    (send gauge set-value 4)
                                                  ;(define doc (com-get-property word "Documents"))
                                                  (define file (open-doc word  (path->string m)))
                                                    (send gauge set-value 5)
                                                 ; (write 1)
                                                  (define tbs (file-get-tables file))
                                                  (define tb (item-ref tbs 1))
                                                  (send page1 fill-word-table word tb)
                                                  (when (send page2 is-shown?)
                                                     (define tb2 (item-ref tbs 2))
                                                    (send page2 fill-word-table word tb2))
                                                  (define out (path->string(make-temp-pdf-file)))
                                                  (word-to-pdf-file file out)
                                                   ; (send gauge set-value 4)
                                                  (close-file-without-save-change file);
                                                    ;(send gauge set-value 5)
                                                  (define mode (get-current-edit-mode))
                                                  (case mode
                                                    [(new)(define id(files_editable:insert (get-cnn) (get-current-subindex) out (send task-name get-value)(send page1 save-meta-data)))
                                                          (send viewer-file-list reload* (get-current-subindex))]
                                                    [(edit)(files_editable:update (get-cnn)
                                                                                  (get-current-edit-key)
                                                                               out
                                                                               (send task-name get-value)
                                                                               (send page1 save-meta-data))
                                                           (send viewer-file-list reload* (get-current-subindex))]
                                                     [else(error 'save "error when save data mode can not be ~a\n" mode)])
                                                     (send gauge set-value 6)
                                                  
                                                    (send gauge set-value 7)
                                                    (enable-new)
                                                    ;(send message-side set-label "保存完毕")
                                                    (set-box! current-edit-state 'success);
                                                    (send gauge set-value 0)
                                                    (send gauge show #f)
                                                    ]
                                 [else (error 'save-file "can not find model-file ~a" m)]))]))
                               
(define cancel-button (new button%[parent editer-tools][label "取消"][vert-margin 20]	 
   	 	[horiz-margin 100][callback (lambda(b e)
                                              (define state(case (unbox current-edit-state)
                                                         [(none success)'yes]
                                                         [else(message-box "提示" "您正在进行的编辑尚未保存!\n是否放弃更改?" main-frame
                                                                            '(yes-no no-icon))]))
                                              (when (eq? state 'yes)
                                                (show-list)
                                                (enable-new)))]))
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
                                            
                                            (set-box! current-edit-key choice-this)
                                            (set-box! current-edit-mode 'edit)
                                            (set-box! current-edit-state 'start)
                                            (send page1 load-meta-data (files_editable:get-metadata (get-cnn) choice-this))
                                            (show-page2*)
                                            ))
                                        )]))
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
;(define message-side(new message%[parent nv-tools][label "     列表"][horiz-margin 10][min-width 90]))
(define gauge (new gauge%
                   (label "进度  ")
                   (parent nv-tools)
                   (range 7)
                   (min-width 100)
                   (stretchable-width #f)))
;(send gauge show #f)
(send page1 end-container-sequence)
;(show-list)
(send main-frame end-container-sequence)
(send gauge show #f)
(define(scroll@x item)
  ;(displayln item)
  ;(displayln (send page1 item-y item))
  (setscrollpos:vertical editer-box (send page1 item-y item)))
(define(scroll@y item)
  ;(displayln item)
  ;(displayln (send page1 item-y item))
  (setscrollpos:vertical editer-box (+ (send page1 get-height)(send page2 item-y item))))

(define(init!)
  (when (>(get-group)0)
    (send nv-tools enable #t)
    (enable-new)))
(define(enable-new)
  (when (>(get-group)1)
    (send new-button enable #t)))
(define(enable-edit)
  (when (>(get-group)0)
    (send edit-button enable #t)))
(define(enable-up)
  (when (>(get-group)1)
    (send up-button enable #t)))
(define(enable-down)
  (when (>(get-group)1)
    (send down-button enable #t)))
(define(enable-delete)
  (when (>(get-group)1)
    (send delete-button enable #t)))
;----------------------------------------------------