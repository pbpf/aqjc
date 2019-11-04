#lang racket
(require "../interface.rkt"
         "private/hash-configurer.rkt"
         "transition-tool.rkt"
         xml)

;储存形式(磁盘模型)



;xml作为配置数据的储存形式
(define xml-configurer%
  (class* hash-table-configurer% (configurer-file<%>)
    (inherit load-table!)
    (inherit-field field-table)
    (init-field file-identification)
    
 (define/public(read-table file-path identification)
      (call-with-input-file   file-path
        (lambda(in)
              (let((r(xml->xexpr(document-element(read-xml in)))))
                (if(list-with-identification? r identification)
                    (xexpr->multiple-hash(cddr r))
                 (error 'read-file    "configure file ~a identification  \n expected: ~a \n given: ~a" file-path identification (car r))
                 )))))
    
    (define/public(check-file-identification file-path)
      (with-handlers([exn:fail? (lambda(e)#f)])
      (call-with-input-file   file-path
        (lambda(in)
              (let((r(xml->xexpr(document-element(read-xml in)))))
                (if(list-with-identification? r file-identification)
                    #t
                 #f
                 ))))))
    
    (define/public(load-file! file-path error-thunk)
      (with-handlers([exn:fail? error-thunk])
       (load-table!
               (read-table file-path file-identification))))
   
    
    (define/public(save-file! file-path #:exists exists-do)
      (let((ready-to-write (cons file-identification (cons '()(multiple-hash->xexpr field-table)))))
      (call-with-output-file  file-path
        (lambda(out)
          (write-xexpr ready-to-write out)
          (void))
         #:exists exists-do)))
    
    (super-new)))


(provide (all-defined-out))