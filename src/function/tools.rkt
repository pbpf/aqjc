#lang racket/base
(require db/base
         racket/date
         racket/port
         racket/path
         racket/file
         racket/class
         racket/string
         openssl/md5
         "private/word_to_pdf.rkt"
         "private/database.rkt")

(provide (all-defined-out))
;
(define(current-sql-date)
  (let((d (current-date)))
  (sql-date (date-year d)
            (date-month d)
            (date-day d))))

(define(string->sql-date str)
  (apply sql-date (map string->number (string-split str "-"))))

(define home-path (build-path (find-system-path 'temp-dir) "aqjc"));
;(define convert-path (build-path home-path "convert"))
(define cache-path (build-path home-path "cache"))
;
;(make-temporary-file
(define(init-path*)
 ;(make-parent-directory* convert-path)
 (make-directory*  cache-path))

(define(make-temp-pdf-file)
  (build-path cache-path	(format "rkttmp~a~a~a.pdf"
                                (random 4294967087)
                                (random 4294967087)
                                (random 4294967087))))

(define(file-path->bytes str)
  
  (port->bytes (open-input-file str)))
;-----------------------file-------------------------------------------------------------------------------------
(define *ONCEREAD* (* 1024 4096))
(define(files:insert cnn subindex file name)
  (files_editable:insert cnn subindex file  name sql-null))
(define(files_editable:insert cnn subindex file  name metadata)
  (define md5sum (md5 (open-input-file file)))
  (define id (query-value cnn "select addfile($1,$2,$3,$4,$5)"
                                                     subindex
                                                     name
                                                     md5sum
                                                     #""
                                                     metadata))
  (when (> id 0)
  (for([bs (in-port (lambda(in)(read-bytes *ONCEREAD* in))(open-input-file file))])
    (displayln (bytes-length bs))
    (query-exec cnn "update aqjcfiles set info=info||$2 where id=$1" id bs)))
   id)
(define(files:swapindex cnn subindex id1 id2)
  (query-value cnn "select swapindex($1,$2,$3)" subindex id1 id2))

(define(insert-fail? id)
  (< id 0))
(define(files:insert* cnn subindex file);gauge<=2
  (define type (bytes->string/utf-8(filename-extension file)))
  (case type
    [("docx" "doc") (define out (make-temp-pdf-file))
              (word-to-pdf* (path->string file) (path->string out))
              ;(sleep 3)
              (define id (files:insert cnn subindex out (path->string(file-name-from-path file))))
              (delete-file out)
              id]
    #|
    [("xlsx")(define out (make-temp-pdf-file))
              (excel-to-pdf* (path->string file) (path->string out))
              ;(sleep 3)
              (define id (files:insert cnn subindex out (path->string(file-name-from-path file))))
              (delete-file out)
              id]
    [("pptx")(define out (make-temp-pdf-file))
              (office-to-pdf* (path->string file) (path->string out) #:type "ppt")
              ;(sleep 3)
              (define id (files:insert cnn subindex out (path->string(file-name-from-path file))))
              (delete-file out)
              id]
|#
    [("pdf") (define out (make-temporary-file "aqjctmp~a.pdf" file cache-path));改成全英文避免路径是中文导致错误
             (define id (files:insert cnn subindex out (path->string(file-name-from-path file))))
             ;(delete-file out)
             id]
    [else (error 'insert-file "can not accept filetype ~a" type)]))

(define(files:move-up cnn id)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex+1 where id=$1"
              id))
(define(files:swap cnn id)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex+1 where id=$1"
              id))
(define(files:move-up-all cnn  subindex)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex+1 where subindex=$1"
              subindex))
(define(files:move-down cnn id)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex-1 where id=$1"
              id))
(define(files:delete cnn id)
 (query-exec cnn "delete from aqjcfiles where id=$1" id))
;---------------------------------------editable-------------------------------------------------------------


;-------------------------------------------
(define(files_editable:update cnn id file name metadata)
  (query-exec cnn "update aqjcfiles set filename=$1,info=$2,metadata=$3 where id=$4"
                                                      name
                                                     (port->bytes  (open-input-file file))
                                                      metadata
                                                      id))

  (define(query-subindex cnn x)
     (query-rows cnn "select id,filename,date from aqjcfiles where subindex=$1 order by sortindex" x))
(define(write-out-file cnn id tz)
 
 (write-bytes (query-value cnn "select info from aqjcfiles where id=$1"  id)
              (open-output-file tz)))
;----                 
(define(files_editable:insert* cnn subindex file metadata)
  (define type (bytes->string/utf-8(filename-extension file)))
  (case type
    [("docx") (define out (make-temporary-file "aqjctmp~a.pdf" #f (current-directory)))
              (word-to-pdf* (path->string file) (path->string out))
              ;(sleep 3)
              (dynamic-wind void
                            (lambda()(files_editable:insert cnn subindex out (path->string(file-name-from-path file)) metadata))
                            (lambda()(delete-file out)))]
    [("pdf") (define out (make-temporary-file "aqjctmp~a.pdf" file cache-path))
             (dynamic-wind void
                            (lambda()(files_editable:insert cnn subindex out (path->string(file-name-from-path file)) metadata))
                            (lambda()(delete-file out)))]
    [else (error 'insert-file "can not accept filetype ~a" type)]))
(define(date-string->date str)
  (define lst(map string->number(string-split str "-")))
  (apply sql-date lst))
(define(mzbg-insert cnn subindex file name metadata errortype date-string)
  (define id (files_editable:insert cnn subindex file name metadata))
 ; (printf "~a\n" id)
  (query-exec cnn "insert into aqjcmzbg(id,errortype,date)values($1,$2,$3)" id errortype (date-string->date date-string)))
(define(mzbg-select-type cnn begin end)
  (define t(for/hash([(i j)(in-query cnn "select errortype,count(*) from aqjcmzbg where date>=$1 and date<=$2 group by errortype"
              (date-string->date begin) (date-string->date end))])
    (values i j)))
  (for/list([i (in-range 0 16)])
    (hash-ref t i (lambda() 0))))
(define(mzbg-update cnn id file name metadata errortype date)
  (files_editable:update cnn id file name metadata)
  (query-exec cnn "update aqjcmzbg set errortype=$1,date=$2" errortype date))
(define(mzbg-delete cnn id)
 (query-exec cnn "delete from aqjcfiles where id=$1" id)
 (query-exec cnn "delete from aqjcmzbg where id=$1" id ))

(define(files_editable:move-up cnn id)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex+1 where id=$1"
              id))
(define(files_editable:move-down cnn id)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex-1 where id=$1"
              id))
(define(files_editable:delete cnn id)
  ;(printf "~a ~a\n" filename subindex)
 (query-exec cnn "delete from aqjcfiles where id=$1"id))
(define(files_editable:move-up-all cnn  subindex)
  (query-exec cnn
              "update aqjcfiles set sortindex=sortindex+1 where subindex=$1"
              subindex))
(define(files_editable:get-metadata cnn  id)
  (query-value cnn "select metadata from aqjcfiles where id=$1" id))
;------------------------------------------------------------------------------
(define(get-cnn)
  (send current-login-context get-cnn))
(define(get-group)
  (send current-login-context get-group))
(define(get-user)
  (send current-login-context get-user))
(define(is-owner?)
  (send current-login-context is-owner))
;----------------------------------------------------------
(define current-subindex* (box 1))
(define(get-current-subindex)
  (unbox current-subindex*)
  )
(define(set-current-subindex! n)
  (set-box! current-subindex* n))

(init-path*)