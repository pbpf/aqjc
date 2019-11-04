#lang racket/base
(require "nav8-base.rkt"
         "private/user.rkt"
         "private/configurator/main.rkt"
         "private/tools.rkt"
         "private/database.rkt"
         "private/main-frame.rkt"
         "tools.rkt"
         racket/class
         racket/gui/base
         racket/path
         racket/date
         racket/file
         db/base)
(provide page2-load-data!)
(define-sub-configurer backup-config (main-config-init-sub 'backup))
(define-backup-config dir   (path->string (find-system-path 'home-dir)))
(define path (build-path dir "aqjc" "backup"))
(make-directory* path)
(send page2 begin-container-sequence)
(define page2-info (new vertical-panel% [parent page2]))
(define tools (new horizontal-panel%[parent page2-info][stretchable-height #f]))
(define nv82 (new list-box-sort-by-header%
               [parent page2-info]
               [columns '("文件名" "日期" "版本" "类型")]))
(define(solve-backupfilename f path n)
  (if(file-exists? (build-path path (format "~a-~a.backup" f n)))
     (solve-backupfilename f path (add1 n))
     (build-path path (format "~a-~a.backup" f n))))
(define(solve-backupfilename* f path)
  (if(file-exists? (build-path path (format "~a.backup" f)))
     (solve-backupfilename f path 1)
     (build-path path (format "~a.backup" f))))
(define new-button (new button% [label "创建"][parent tools]
                        [callback (lambda(b e)(begin-busy-cursor)(define filename (get-text-from-user "输入" "请输入备份文件名称" ))
                                              (define realusename
                                                (if (and filename (not(string=? filename "")))
                                                    (solve-backupfilename* filename path)
                                                    (solve-backupfilename* (parameterize([date-display-format 'iso-8601])
                                                                   (date->string (current-date))) path)))
                                             (define cnn (create-sqlite3 realusename))
                                               (init-sqlite3-table! cnn)
                                    
                                           (backup-postgresql-to-sqlite3-all (get-cnn) cnn)
                                    
                                          (disconnect cnn)
                                    (page2-load-data!)
                                    (end-busy-cursor))]))
(define delete-button(new button% [label "删除"][parent tools]
                          [callback (lambda(b e)(define cnn (get-cnn))
                                 (define cc (send nv82 get-selections))
                                 (unless (null? cc)
                                   (for([i (in-list cc)])
                                   (define name (send nv82 get-data i))
                                   (define file (build-path path name))
                                   (delete-file file))
                                   (page2-load-data!)))]))
(define restore-button(new button% [label "恢复"][parent tools]
                          [callback (lambda(b e)(define cnn (get-cnn))
                                 (define cc (send nv82 get-selections))
                                 (when (= 1 (length cc))
                                 (define name (send nv82 get-data (car cc)))
                                   (define file (build-path path name))
                                   (define cnn  (get-sqlite3 file))
                                   (restore-sqlite3-to-postgresql cnn (get-cnn))
                                   (disconnect cnn)
                                   (message-box "信息" "恢复成功")))]))
(define setting-button(new button% [label "设置"][parent tools]
                           [callback(lambda(b e)(define cdir (get-directory	"请选择备份文件夹" main-frame  dir))
                                      (when cdir
                                        (set-backup-config! 'dir  (path->string cdir))
                                        (save-main-config!)
                                        (message-box "信息" "设置成功,重启后生效")))]))
                                                        
(send page2 end-container-sequence)
(define(scan-dir dir)
  (for/list([p(in-directory dir)]
            #:when (and (good-backup-file? p)(path-has-extension?  p  ".backup")))
    (append (list (format "~a" p)
                  (format "~a" (file-name-from-path p))) (get-backup-info p))))

(define(page2-load-data!)
  (define lst (scan-dir path))
  (send nv82 load-data
        (for/list([i (in-list lst)])
          (list->vector (cdr i)))))
; (send main-frame show #t)
;(send nv-info active-child page2)