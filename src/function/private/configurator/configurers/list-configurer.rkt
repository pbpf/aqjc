#lang racket
(require "../interface.rkt"
         "private/hash-configurer.rkt"
         "transition-tool.rkt"
         )

;储存形式(磁盘模型)

;表作为配置数据的储存形式
(define list-configurer%
  (class* hash-table-configurer% (configurer-file<%>)
    (inherit-field field-table)
    (init-field file-identification)
    
    (define/private(read-file file-path)
      (call-with-input-file   file-path
        (lambda(in)
              (let((r(read in)))
                (if(not(list? r))
                   (error 'read-file!    "configure file ~a except a list which is isn't" file-path )
                (if(list-with-identification? r file-identification)
                   (cdr r)
                 (error 'read-file!    "configure file ~a identification  \n expected: ~a \n given: ~a" file-path file-identification (car r))
                    ))))))
    
    (define/public(load-file! file-path)
      (set! field-table
               (list->multiple-hash(read-file file-path))))
    
    
    (define/public(save-file! file-path)
      (let((ready-to-write      (begin(when(file-exists? file-path)
                                          (read-file file-path))
                                (cons file-identification (multiple-hash->list field-table)))))
      (call-with-output-file  file-path
        (lambda(out)
          (write ready-to-write out))
        #:exists 'replace)))
    
    (super-new)))

(provide (all-defined-out))

