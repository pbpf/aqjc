#lang racket
 (require scribble/core
           scribble/scheme
           scribble/manual
           racket/list
           scribble/text
           scribble/html-properties
           scribble/latex-properties
           "style.rkt"
           (for-label scheme/gui/base))
(define(string-length* str)
  (for/fold([sum1 0])
           ([i (in-string str)])
    (if(char=? #\tab i)
       (*(+ 1 (quotient  sum1 8))8)
       (+ sum1 1))))
(define(table-length str)
  (define len(string-length* str))
    (-(*(+ 1 (quotient  len 8))8) len))
(define(hslen s str)
  (case s
   [(" ")1]
   [("\t")(table-length str)]))
               
(provide  system    tree-layout2d->table sys small-style)




(struct tree-layout2d (elem nodes2d))
(struct tree-node2d (elem nodes))
(define(tree-layout2d-max-length s)
  (for/fold([max1 0])
           ([i (in-list (tree-layout2d-nodes2d s))])
    (max max1 (tree-node2d-length i))))
(define(tree-node2d-length k)
  (length (tree-node2d-nodes k)))
(define(tree-node2d-pad-to ns len)
  (struct-copy  tree-node2d ns
               [nodes (append (tree-node2d-nodes ns)(make-list (- len (tree-node2d-length ns)) 'cont))]))
(define(node-length s)
   (if(paragraph? s)
      (node-length(paragraph-content s))
  (content-width  s)))
(define(max-length-node2d s)
  (for/fold([max1 (node-length (tree-node2d-elem s))])
           ([i (in-list (tree-node2d-nodes s))])
    (max max1 (node-length i))))
(define(solve-pad tree #:prefix [prefix 10]  #:scale [s 1] #:prefix2 [p2 5])
  (cons
   (hspace p2)
   (for/list ([i (in-list (drop-right(tree-layout2d-nodes2d tree) 0))])
     ;(displayln (max-length-node2d i))
    (hspace(+ prefix (* (max-length-node2d i) s))))))
(define(normalfy s)
  ;(displayln s)
  (define len (tree-layout2d-max-length s))
  ;(displayln s)
  (struct-copy  tree-layout2d s
               [nodes2d
                (for/list([i (in-list (tree-layout2d-nodes2d s))])
                  (tree-node2d-pad-to i len))]))

(define(tree-to-table ls)
  ;(displayln ls)
  (apply map (cons list (map tree-node2d-nodes (tree-layout2d-nodes2d ls)))))
(define(table-row-add-between t h)
  (for/list([i (in-list t)])
    (add-between i (paragraph (style #f '()) h))))


(define(tree-layout2d->table tree #:sep [sep (hspace 0)] #:pad-left [pad-left 2] )
  (define pad (+ 6(* (length (tree-layout2d-nodes2d tree)) 4)))
  (define tree* (tree-to-table(normalfy tree)))
 ; (displayln (map tree-node2d-elem (tree-layout2d-nodes2d tree)))
  (make-table (make-style #f '())
  (list (list(make-paragraph(make-style #f '())(list (hspace pad)(tree-layout2d-elem tree) (hspace pad))))
        (list(make-paragraph(make-style #f '())(list (hspace pad-left ) (string-append (make-string  (+ pad 2 (- pad-left)) #\_)"|" (make-string (+ pad 7 (- pad-left)) #\_))
                                                     (hspace pad-left))))
        (list(make-paragraph(make-style #f '())(add-between(solve-pad tree #:prefix 1 #:prefix2  2 #:scale 1) "|")))
        
  (list(make-table(make-style #f '())
              (cons (add-between (map tree-node2d-elem  (tree-layout2d-nodes2d tree)) (make-paragraph(make-style #f '())(hspace 1)))
              (table-row-add-between tree* (hspace 1))))))))
(define(tree-node2d-layout node #:sep [sep (hspace 0)] #:pad-left [pad-left (hspace 1)] #:pad-to [pad-to 10])
  (map (lambda(nd)(list (make-paragraph (style #f '()) pad-left)
                                   (make-paragraph (make-style #f '()) "|-")
                                   (make-paragraph (style #f '()) sep)
                                   nd))
                  (tree-node2d-nodes node)))


(define yxgf
    (tree-node2d
     (make-paragraph bigger-style "运行规范")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1)"|-"(make-element small-style  x))))
          (list "上级指示" "工作流程" "建设标准" "岗位职责" "监察单" "法规依据" "在职学习" "岗位认定" "奖惩制度"))))
(define gzjh
    (tree-node2d
     (make-paragraph  bigger-style "工作计划")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list(hspace 1) "|-"(make-element small-style  x))))
          (list "年度计划" "月份计划" "日计划" "专项计划" "在职学习计划" "法规考核计划"))))
(define gzjh2
    (tree-node2d
     (make-paragraph  bigger-style "监察实施")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1)"|-"(make-element small-style  x))))
          (list "下达任务" "监察准备" "检查开展" "问题查处" "取证记录" "督促整改" "整改反馈通知"))))
(define gzjh3
    (tree-node2d
     (make-paragraph bigger-style"分析预测")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1)"|-"(make-element small-style  x))))
          (list "问题汇总" "量化分析" "形势分析" "研究措施" "安全预警"))))
(define gzjh4
    (tree-node2d
     (make-paragraph  bigger-style "总结报告")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1) "|-"(make-element small-style  x))))
          (list "监察日报" "监察月报" "年度总结" "安全形势分析纪要" "免责报告分析报告"))))
(define gzjh5
    (tree-node2d
     (make-paragraph  bigger-style "专项工作")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1)"|-"(make-element small-style  x))))
          (list "安全形势分析" "免责报告填写系统" "安全风险评估" "照相管理" "质量检验" "安全奖惩" "安全征文"))))
(define gzjh6
    (tree-node2d
     (make-paragraph bigger-style "教育宣传")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1) "|-"(make-element small-style  x))))
          (list "质量安全教育" "事故问题统计" "预防认为差错" "故障机理研究" "精细化管理相关理论" "航空维修保障研究"))))
(define gzjh7
    (tree-node2d
     (make-paragraph  bigger-style "用户系统")
     (map (lambda(x)(make-paragraph (make-style #f '()) (list (hspace 1)"|-"(make-element small-style  x))))
          (list "用户管理" "备份还原" "文档说明" "意见反馈"))))

(define sys (tree-layout2d   "系统"
                            (list yxgf gzjh gzjh2 gzjh3 gzjh4 gzjh5 gzjh6 gzjh7)))
;(tree-layout2d->table sys)
                  

;(normalfy sys)