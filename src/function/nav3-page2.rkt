#lang racket/base
(require "nav3-base.rkt"
         racket/date
         racket/class
        ; racket/gui/base
        ; "meta-jcss-struct.rkt"
         "private/table.rkt"
         "private/table-cell-mixin.rkt"
         (prefix-in t: "private/table-cell-class.rkt")
         )
(provide p7)
(send page2 begin-container-sequence)
(define word:constant% (word-table-cell-mixin t:constant%))
(define word:text% (word-table-cell-mixin t:text%))
(define word:date% (word-table-cell-mixin t:date%))
(define(make-const pos merge label )
  (new word:constant%  [label label][pos pos][merge merge][auto-resize #t]
  [parent page2]))
(define(make-text pos merge  #:init [init ""])
  (new word:text% [pos pos][merge merge][parent page2][init-value init]))

(define p7 (make-const (pos 1 1)(colmerge 1 6) "整改通知单与反馈单"))
(void (make-const (pos 2 1)(colmerge 1 6) "航空维修安全问题整改单(指令联)"))
(void (make-const (pos 3 1)(colmerge 1 1) "时        间"))
(void (new word:date%[parent page2][pos (pos 3 2)][merge (colmerge 2 2)][callback (lambda(b e)
                                                                                 (send t1 set-value (send b get-value)))])) 
(void (make-const (pos 3 3)(colmerge 3 3) "编号"))
(void (new word:text% [pos (pos 3 4)][merge (colmerge 4 6)][parent page2][callback (lambda(b e)
                                                                                            (send h1 set-value (send b get-value)))]))
(void (make-const (pos 4 1)(colmerge 1 1)"被告知单位/人"))
(void (new word:text% [pos (pos 4 2)][merge (colmerge 2 2)][parent page2][callback (lambda(b e)
                                                                                            (send t2 set-value (send b get-value)))]))
(void (make-const (pos 4 3)(colmerge 3 3)"整改时限"))
(void (new word:text% [pos (pos 4 4)][merge (colmerge 4 4)][parent page2][callback (lambda(b e)
                                                                                            (send t3 set-value (send b get-value)))]))
(void (make-const (pos 4 5)(colmerge 5 5) "责任人"))
(void (new word:text% [pos (pos 4 6)][merge (colmerge 6 6)][parent page2][callback (lambda(b e)
                                                                                            (send t4 set-value (send b get-value)))]))
(void (make-const (pos 5 1)(colmerge 1 1) "问题情况"))
(void  (new word:text% [pos (pos 5 2)][merge (colmerge 2 6)][parent page2]
           [style '(multiple)]))
(void (make-const (pos 6 1)(colmerge 1 1) "整改建议"))
(void  (new word:text% [pos (pos 6 2)][merge (colmerge 2 6)][parent page2]
           [style '(multiple)]))
(void (make-const (pos 7 1)(colmerge 1 1) "抄       送"))
(void  (new word:text% [pos (pos 7 2)][merge (colmerge 2 6)][parent page2]))
;----------------------------------------------
(void (make-const (pos 8 1)(colmerge 1 6) "航空维修安全问题整改单(反馈联)"))
(void (make-const (pos 9 1)(colmerge 1 1) "时        间"))
(define t1 (new word:text%[parent page2][pos (pos 9 2)][merge (colmerge 2 2)][init-value (parameterize[(date-display-format 'iso-8601)]
                                                            (date->string (current-date)))]))
(void (make-const (pos 9 3)(colmerge 3 3) "编号"))
(define h1 (make-text (pos 9 4)(colmerge 4 6)))
(void (make-const (pos 10 1)(colmerge 1 1)"被告知单位/人"))
(define t2 (make-text (pos 10 2)(colmerge 2 2)))
(void (make-const (pos 10 3)(colmerge 3 3)"整改时限"))
(define t3 (make-text (pos 10 4)(colmerge 4 4)))
(void (make-const (pos 10 5)(colmerge 5 5) "责任人(签字)"))
(define t4 (make-text (pos 10 6)(colmerge 6 6)))
(void (make-const (pos 11 1)(colmerge 1 1) "整改情况"))
(void  (new word:text% [pos (pos 11 2)][merge (colmerge 2 6)][parent page2]
           [style '(multiple)]))
(void (make-const (pos 12 1)(colmerge 1 1) "被告知人/单位\n的意见、建议"))
(void  (new word:text% [pos (pos 12 2)][merge (colmerge 2 6)][parent page2]
           [style '(multiple)]))
(void (make-const (pos 13 1)(colmerge 1 1) "其他人/单位\n对整改情况的\n意见、建议"))
(void  (new word:text% [pos (pos 13 2)][merge (colmerge 2 6)][parent page2][style '(multiple)]))
(send page2 end-container-sequence)
;(send test-frame show #t)
(send file-list-and-viewer set-percentages '(1/3 2/3))