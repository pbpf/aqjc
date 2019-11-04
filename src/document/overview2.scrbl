#lang racket
 (require scribble/struct
           scribble/scheme
           scribble/manual
           (for-label scheme/gui/base))
(provide diagram->table system short-windowing-diagram)
(define (diagram->table d)
    (make-table
     #f
     (map (lambda (line)
            (list (make-flow 
                   (list (make-paragraph 
                          (let loop ([line line])
                            (cond
                             [(regexp-match #rx"(.*)( +)(.*)" line)
                              => (lambda (m)
                                   (append (loop (cadr m))
                                           (list (hspace (string-length (caddr m))))
                                           (loop (cadddr m))))]
                             [(regexp-match #rx"([^-a-zA-Z0-9]*)([-a-zA-Z0-9<%>]+)(.*)" line)
                              => (lambda (m)
                                   (append (loop (cadr m))
                                           (list (to-element (make-just-context (string->symbol (caddr m))
                                                                                #'here)))
                                           (loop (cadddr m))))]
                             [else (list (make-element 'tt (list line)))])))))))
          (regexp-split #rx"[\r\n]+" d))))
(define system
#<<DIAG
                                        系统
       __________________________________|__________________________________
      |         |         |         |         |         |         |         |
   运行规范   工作计划   监察实施   分析预测   总结报告   专项工作   教育宣传   用户系统
      
DIAG
  )
(define short-windowing-diagram
#<<DIAG
                           area<%>
       ______________________|_______________
       |                  |                 |
  subarea<%>          window<%>      area-container<%>      
       |____       _______|__________       |
            |      |                |       |
           subwindow<%>          area-container-window<%>
        ________|________                |
        |               |                |
     control<%>       canvas<%>   top-level-window<%>
DIAG
)









