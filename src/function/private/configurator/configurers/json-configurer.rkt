#lang racket
(require "../interface.rkt"
         "private/hash-configurer.rkt"
         "transition-tool.rkt"
         json)

;储存形式(磁盘模型)

;json作为配置数据的储存形式
(define json-configurer%
  (class* hash-table-configurer% (configurer-file<%>)
    (inherit-field field-table)
    (init-field file-identification)
    
    (define/public(read-table file-path)
      (call-with-input-file   file-path
        (lambda(in)
              (let((r(read-json in)))
                (if(and (jsexpr? r)(list-with-identification? r file-identification))
                    (cadr r)
                 (error 'read-file!   "configure file ~a identification  \n expected: ~a \n given: ~a" file-path file-identification (car r))
                    )))))
    
    (define/public(load-file! file-path)
      (set! field-table
               (read-table file-path)))

    (define/public(save-file! file-path #:exists exists-do)
      (let((ready-to-write (list file-identification  field-table )))
      (call-with-output-file  file-path
        (lambda(out)
          (write-json ready-to-write out))
        #:exists exists-do)))
    
    (super-new)))

(provide (all-defined-out))