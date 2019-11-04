#lang racket/base
(require 
         racket/gui/base
       ;  racket/date
         racket/class
         racket/runtime-path
         racket/path
         racket/set
         db/base
         racket/file
        ; "meta-jcss-struct.rkt"
         "private/main-frame.rkt"
         "private/draw.rkt"
         "private/table.rkt"
         "private/writing-word.rkt"
         "private/word_to_pdf.rkt"
         "nav4-base.rkt"
        ; "private/preferences.rkt"
         "private/list-box.rkt"
         "private/scrollbar.rkt"
         "tools.rkt"
        (prefix-in word: "private/table-cell-class.rkt")
         "private/panel.rkt")
(provide scroll@x reload-nv4 p1 p2 p3 p4 p5 init!)
(define(reload-nv4)
  (send viewer-file-list reload* (get-current-subindex)))
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
;(provide (rename-out [base-panel nv4]))
;(define test-frame (new frame% [label ""][width 700][height 600]))
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
                           (send up-button   enable #f)
                           (send down-button enable #f)
                           ])
                   )]
       [reload (lambda(x)(send viewer-file-list
                               load-multi-choices(query-subindex (get-cnn) x)))]
       [columns '("文件名" "日期")]
       [columns-types '("text" "date")]))

;(define base-panel  editer-box)

(define fxyc (new merge-table-panel% [parent editer-box][columns 5][rows 25][style '(border)]))
(define editer-tools (new horizontal-panel%[parent editer-box][alignment '(center center)]))
(send fxyc begin-container-sequence)
(define(make-const pos merge label[min-width #f][min-height #f] )
  (word:make-const pos merge label fxyc min-width min-height #:panel-style '(border))
  )
(define(make-text pos merge  #:init [init ""] #:call [call (lambda( b e)(void))] #:style [style '(single)] #:min-height [min-height #f])
  (word:make-text pos merge fxyc #:init init #:call call #:style style #:min-height min-height #:panel-style '(border)))

(void (make-const (pos 1 1)(colmerge 1 5) "分析预测"))
(define p1 (make-const (pos 2 1)(colmerge 1 5) "问题汇总"))
(void (make-const (pos 3 1)(colmerge 1 1) "时间"))
(void (word:make-date (pos 3 2) (colmerge 2 2) fxyc #:panel-style '(border)))
(void (make-const (pos 3 3)(colmerge 3 3) "内容"))
(define task-name (make-text (pos 3 4)(colmerge 4 5) #:init "?月安全监察发现问题汇总"))
(void (make-const (pos 4 1)(colmerge 1 1) "编号"))
(void (make-const (pos 4 2)(colmerge 2 2) "问题性质"))
(for ([i (in-range 1 11)])
  (make-const (pos (+ 4 i) 1)(colmerge 1 1) (format "~a" i)))
(void (make-const (pos 4 3)(colmerge 3 4) "问题类别"))
(void (make-const (pos 4 4)(colmerge 5 5) "数量"))
(void (make-const (pos 5 2)(vector 5 8 2 2)"一般问题"))
(void (make-const (pos 9 2)(vector 9 11 2 2)"较大问题"))
(void (make-const (pos 12 2)(vector 12 14 2 2)"重大问题"))
(void (make-const (pos 5 3)(colmerge 3 4) "现场秩序不正规" 300))
(void (make-const (pos 6 3)(colmerge 3 4) "一般违章问题"))
(void (make-const (pos 7 3)(colmerge 3 4) "一般安全隐患"))
(void (make-const (pos 8 3)(colmerge 3 4) "登统计不及时或错误"))
(void (make-const (pos 9 3)(colmerge 3 4) "组织流程不规范"))
(void (make-const (pos 10 3)(colmerge 3 4) "违章指挥、违章操作"))
(void (make-const (pos 11 3)(colmerge 3 4) "安全设施设备不完好"))
(void (make-const (pos 12 3)(colmerge 3 4) "现场组织管理混乱"))
(void (make-const (pos 13 3)(colmerge 3 4) "技术准备不充分、违规违章操作"))
(void (make-const (pos 14 3)(colmerge 3 4) "安全措施不落实"))
(define(sum-lst* lst)
  (for/sum ([i (in-list lst)])
    (define t
     (string->number (send i get-value)))
    (if t t 0)))
(define(good-num? s)
  (define len (string-length s))
  (or (= len 0)(char<=? #\0 (string-ref s (sub1 len)) #\9)))
(define(del-char s)
  (define len (string-length s))
  (substring s 0 (max 0 (sub1 len))))
               
(define lst1
  (list 
 (make-text (pos 5 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s1 set-label (format "~a" (sum-lst* lst1)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
 (make-text (pos 6 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s1 set-label (format "~a" (sum-lst* lst1)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
  (make-text (pos 7 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s1 set-label (format "~a" (sum-lst* lst1)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
(make-text (pos 8 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s1 set-label (format "~a" (sum-lst* lst1)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))))
(define lst2
  (list
   (make-text (pos 9 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s2 set-label (format "~a" (sum-lst* lst2)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
 (make-text (pos 10 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s2 set-label (format "~a" (sum-lst* lst2)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
 (make-text (pos 11 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s2 set-label (format "~a" (sum-lst* lst2)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))))
(define lst3
  (list
 (make-text (pos 12 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s3 set-label (format "~a" (sum-lst* lst3)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
(make-text (pos 13 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s3 set-label (format "~a" (sum-lst* lst3)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))
(make-text (pos 14 4)(colmerge 5 5)#:init "0" #:call (lambda(b e)
                                                       (cond
                                                         [(good-num? (send b get-value))
                                                 
                                                                 (send s3 set-label (format "~a" (sum-lst* lst3)))
                                                                    (draw-this*)]
                                                         [else (send b set-value (del-char (send b get-value)))])))))
(void (make-const (pos 15 1)(vector 15 17 1 1) "问题合计"))
(void (make-const (pos 15 2)(colmerge 2 2) "一般问题"))
(void (make-const (pos 16 2)(colmerge 2 2) "较大问题"))
(void (make-const (pos 17 2)(colmerge 2 2) "重大问题"))
(define s1 (make-const (pos 15 3) (colmerge 3 3) "0" 40 60))
(define s2 (make-const (pos 16 3) (colmerge 3 3) "0" #f 60))
(define s3 (make-const (pos 17 3) (colmerge 3 3) "0" #f 60))
;
(define(draw-this*)
  (draw-pict (sum-lst* lst1)
             (sum-lst* lst2)
             (sum-lst* lst3)))
(define(draw-pict a b c)
  (define bm(draw-bm 450 200 a b c))
  (define f(make-temporary-file "aqjc~a.png"))
  (send bm save-file f 'png)
  (send p*1 load-file f))
(define p*1 (word:make-pict (pos 15 4)(vector 15 17 4 5)fxyc 350  180 #:panel-style '(border)))
(define p2 (make-const (pos 18 1)(colmerge 1 5) "量化分析"))
(void (make-const (pos 19 1)(colmerge 1 1) "问题趋势与发生规律"))
(void (make-text  (pos 19 2) (colmerge 2 5)#:style '(multiple)))
(define p3 (make-const (pos 20 1)(colmerge 1 5) "形势分析"))
(void (make-const (pos 21 1)(colmerge 1 1) "监察重点与管理意见"))
(void (make-text  (pos 21 2) (colmerge 2 5)#:style '(multiple)))
(define p4 (make-const (pos 22 1)(colmerge 1 5) "研究措施"))
(void (make-const (pos 23 1)(colmerge 1 1) "预防措施及控制方案"))
(void (make-text  (pos 23 2) (colmerge 2 5)#:style '(multiple)))
(define p5 (make-const (pos 24 1)(colmerge 1 5) "安全预警"))
(void (make-const (pos 25 1)(colmerge 1 1) "产生后果及防范重点"))
(void (make-text  (pos 25 2) (colmerge 2 5)#:style '(multiple)))

(send fxyc end-container-sequence)






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
(define-runtime-path model-path "private/model/nv4.docx")
(define save-button (new button%[parent editer-tools][label "保存"][vert-margin 20]	 
   	 	[horiz-margin 100]
                   [callback (lambda(b e)
                               (define m model-path) 
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
                                                  (send fxyc fill-word-table word tb)
                                                  (define out (path->string(make-temp-pdf-file)))
                                                  (word-to-pdf-file file out)
                                                   ; (send gauge set-value 4)
                                                  (close-file-without-save-change file);
                                                    ;(send gauge set-value 5)
                                                  (define mode (get-current-edit-mode))
                                                  (case mode
                                                    [(new)(define id(files_editable:insert (get-cnn) (get-current-subindex) out (send task-name get-value)(send fxyc save-meta-data)))
                                                          (send viewer-file-list reload* (get-current-subindex))]
                                                    [(edit)(files_editable:update (get-cnn)
                                                                                  (get-current-edit-key)
                                                                               out
                                                                               (send task-name get-value)
                                                                               (send fxyc save-meta-data))
                                                           (send viewer-file-list reload* (get-current-subindex))]
                                                     [else(error 'save "error when save data mode can not be ~a\n" mode)])
                                                     (send gauge set-value 6)
                                                  
                                                    (send gauge set-value 7)
                                                    (send new-button enable #t)
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
                                            (set-box! current-edit-key choice-this)
                                            (set-box! current-edit-mode 'edit)
                                            (set-box! current-edit-state 'start)
                                            (send fxyc load-meta-data (files_editable:get-metadata (get-cnn) choice-this))))
                                        )]))
(define new-button (new button% [parent nv-tools][label "新建"]
                        [callback (lambda(b e)
                                        (show-edit)
                                        (set-box! current-edit-key -1)
                                        ;(send message-side set-label "     新建")
                                        ;(send task-name enable #t)
                                        ;(send new-button enable #f)
                                        ;(send edit-button enable #f)
                                    (set-box! current-edit-mode 'new)
                                    (set-box! current-edit-state 'start)
                                        )]))
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
;(define message-side(new message%[parent nv-tools][label "     列表"][horiz-margin 10][min-width 90]))
(define gauge (new gauge%
                   (label "进度  ")
                   (parent nv-tools)
                   (range 7)
                   (min-width 100)
                   (stretchable-width #f)))


(define(init!)
 ; (printf "init...~a\n" (get-group))
  (when (>(get-group)1)
    (send nv-tools enable #t)))
(define(enable-new)
  (when (>(get-group)1)
    (send new-button enable #t)))
(define(enable-edit)
  (when(>(get-group)1)
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
(define(scroll@x item)
  ;(displayln item)
  ;(displayln (send page1 item-y item))
  (setscrollpos:vertical editer-box (send fxyc item-y item)))
(send gauge show #f)
(send file-list-and-viewer set-percentages '(1/3 2/3))
;(send test-frame show #t)