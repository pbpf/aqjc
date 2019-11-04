#lang racket/base
(require ;"private/database.rkt"
         "private/preferences.rkt"
         "private/main-frame.rkt"
         "private/list-box.rkt"
         "private/pdf-render.rkt"
         "private/active-unit.rkt"
         "nav7-base.rkt"
         "tools.rkt"
         racket/gui/base
         racket/class
         racket/string
        ; racket/file
        ; racket/port
         racket/path
         db/base)
(send main-frame begin-container-sequence)
(define viewer-file-list
  (new list-box-sort-able%
       [parent file-list-panel]
      ; [multi-choices '()];(query-rows (get-cnn) "select filename,date from files where subindex=$1 order by sortindex" (current-subindex))
       [on-dclick (lambda(x)
                    ;(printf "~a\n" x)
                    (define t (make-temp-pdf-file))
                    (write-out-file (get-cnn) (vector-ref x 0)t )
                    (show-file-viewer)
                    (send file-viewer load-file* t)
                    )]
       [on-click (lambda(x);(printf "~a\n" x)
                   (cond
                     [(null? x)(send delete-button enable #f)
                               (send up-button enable #f)
                               (send down-button enable #f)
                               ;(send edit-button enable #f)
                               ]
                     [(null? (cdr x))(enable-delete)
                                     (enable-up)
                                     (enable-down)
                                     ]
                     [else (enable-delete)
                           (send up-button enable #f)
                           (send down-button enable #f)
                           ])
                   )]
       [reload (lambda(x)(send viewer-file-list
                               load-multi-choices(query-subindex (get-cnn) x)))]
       [columns '("文件名" "日期")]
       [columns-types '("text" "date")]))
(define file-viewer (new pdf-viewer-panel%
                    [parent file-list-and-viewer]
                    [style '(border)]))

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
(define insert-button (new button% [parent nv-tools][label "导入"]
                           [callback (lambda( b e)
                                       (define flst(get-file-list #f #f #f #f #f '() '(("pdf file or word file" "*.pdf;*.docx")("Any" "*.*" ))))
                                        (when flst
                                       (send gauge set-range (* (length flst) 7))
                                       (define fas(for/fold([fails '()])
                                               ([f (in-list flst)]
                                                [i (in-naturals 0)])
                                         (define bef (* i 7))
                                         (files:move-up-all (get-cnn) (get-current-subindex))
                                         (send gauge set-value (+ bef 3))
                                         (send  gauge refresh)
                                         (send gauge show #t)
                                         (define id (files:insert* (get-cnn) (get-current-subindex) f))
                                         (cond
                                           [(insert-fail? id)
                                             (send gauge set-value (+ bef 7))
                                             (cons f fails)
                                             ]
                                           [else 
                                             (send gauge set-value (+ bef 7))
                                             fails])))
                                         ;(write(query-value (get-cnn) "select currval('files_id_seq')"))
                                        ; (printf "id ~a\n" id)
                                          
                                          (define cnum(message-box/custom	 "导入结果"
                                                       (format "成功~a个;忽略~a个同名文件" (- (length flst) (length fas))(length fas))
                                                       "确定" "查看" #f))
                                           (when (= cnum 2)
                                            (define t (new dialog% [label "结果"]))
                                            (define txtlist(for/list ([i (in-list flst)]
                                                               [j (in-naturals 1)]
                                                               )
                                                      (if (member i fas)
                                                          (format "[~a]~a: 已有同名文件,忽略" j i)
                                                          (format "[~a]~a: 成功" j i))))
                                            (new text-field%
                                                 	[label #f]
                                                   [parent t]
                                                   [style '(multiple)]
                                                   [init-value (string-join txtlist "\n")])
                                            (send t show #t)
                                             )
                                          (send viewer-file-list reload* (get-current-subindex))
                                          (send gauge show #f)
                                         ))]))
(define(reset-enable)
  (send delete-button enable #f)
  (send up-button enable #f)
  (send down-button enable #f))
(define gauge (new gauge%
                   (label "进度  ")
                   (parent nv-tools)
                   (range 7)
                   (min-width 100)
                   (stretchable-width #f)))
;-------------------------------------------------------------------------------------------------------------
(define abutton% (active-button-mixin% button%))
(define side-color  '( "gray" "CornflowerBlue"))
(for([i (in-list '("质量安全教育" "事故问题统计" "预防人为差错""故障机理研究""精细化管理相关理论""航空维修保障研究"))]
     [j (in-range 71 79)])
  (new abutton%[parent nv-side][colors side-color]
           [label i]
           [min-height side-button-height][min-width side-button-width]
                         [callback (lambda(b e)(set-current-subindex! j)
                                     (send file-viewer reset)
                                     (init!)
                                     (reset-enable)
                                     (send viewer-file-list reload* j))]))
(send main-frame end-container-sequence)
(define(init!)
 ; (printf "init...~a\n" (get-group))
  (when (>(get-group)1)
    (send nv-tools enable #t)))
(define(enable-insert)
  (when (>(get-group)1)
    (send insert-button enable #t)))
(define(enable-up)
  (when (>(get-group)1)
    (send up-button enable #t)))
(define(enable-down)
  (when (>(get-group)1)
    (send down-button enable #t)))
(define(enable-delete)
  (when (>(get-group)1)
    (send delete-button enable #t)))
(send gauge show #f)
(send file-list-and-viewer set-percentages '(1/3 2/3))