#lang racket/base
(require ffi/com)

(define wdAlertsAll      -1);             =-1         # from enum WdAlertLevel
(define wdAlertsMessageBox        -2);    =-2         # from enum WdAlertLevel
(define wdAlertsNone  0)
(provide word-to-pdf* word-to-pdf-file office-to-pdf* excel-to-pdf*)
(define(word-open-file doc filename
                       #:ConfirmConversions [ConfirmConversions com-omit]
                       #:ReadOnly [ReadOnly com-omit]
                       #:AddToRecentFiles [AddToRecentFiles com-omit]
                       #:PasswordDocument [PasswordDocument com-omit]
                       #:PasswordTemplate [PasswordTemplate com-omit]
                       #:Revert [Revert com-omit]
                       #:WritePasswordDocument [WritePasswordDocument com-omit]
                       #:WritePasswordTemplate [WritePasswordTemplate com-omit]
                       #:Format [Format com-omit]
                       #:Encoding [Encoding com-omit]
                       #:Visible [Visible com-omit]
                       #:OpenAndRepair [OpenAndRepair com-omit]
                       #:DocumentDirection [DocumentDirection com-omit]
                       #:NoEncodingDialog [NoEncodingDialog com-omit]
                       #:XMLTransform  [XMLTransform  com-omit])
   (com-invoke doc "OpenNoRepairDialog" (box filename)
               (box ConfirmConversions)
               (box ReadOnly)
              (box AddToRecentFiles)
              (box PasswordDocument)
               (box PasswordTemplate)
              (box Revert)
               (box WritePasswordDocument)
               (box WritePasswordTemplate)
               (box Format)
               (box Encoding)
               (box Visible)
               (box OpenAndRepair)
               (box DocumentDirection)
               (box NoEncodingDialog)
               (box XMLTransform)
               ))
(define(office-to-pdf doc in out)
  (define file (word-open-file doc in #:ReadOnly #t));只读打开
  (com-invoke file "ExportAsFixedFormat"  out 17)
  (com-invoke file "Close" (box 0)))

(define(office-to-pdf-file file out);只读打开
  (com-invoke file "ExportAsFixedFormat"  out 17))
(define(word-to-pdf-file file out);只读打开
  (com-invoke file "ExportAsFixedFormat"  out 17))
  ;(com-invoke file "Close")
;(define IID_format
 ; (string->iid "{5D7E6F43-3E57-353C-95E1-52E9783BE2BE}"))
(define(open-word-auto)
  (with-handlers ([exn:fail? (lambda(x)(values (com-create-instance "Word.Application")
                                               #t))])
    (values (com-get-active-object "Word.Application")
            #f)))
(define(word-to-pdf* in out)
  (define-values(word created?) (open-word-auto))
  (com-set-property! word "DisplayAlerts" wdAlertsNone)
  (define doc (com-get-property word "Documents"))
  (office-to-pdf doc in out)
  (when created?
    (com-invoke word "Quit")
     (com-release word)))

(define(word-to-pdf-list* inlst out-name-proc)
  (define word (com-create-instance "Word.Application"))
  (com-set-property! word "DisplayAlerts" wdAlertsNone)
  (define doc (com-get-property word "Documents"))
  (dynamic-wind (lambda()(void))
                (lambda()
                  (for/list([in (in-list inlst)])
                    (printf "~a\n" in)
                    (list in
                          (with-handlers([exn:fail? (lambda(x)#f)])
                            (define out (out-name-proc in))
                            (office-to-pdf doc in out)
                            out))))
      (lambda()
  (com-invoke word "Quit")
  (com-release word))))

(define(excel-to-pdf* in out)
  (define office (com-create-instance "Excel.Application"))
  (define doc(com-get-property office "Workbooks"))
  (define file (com-invoke doc "open" in com-omit #t))
  (com-invoke file "ExportAsFixedFormat"  0 out 17)
  (com-invoke file "Close")
  (com-release office))
(define(office-to-pdf* in out #:type str)
  (define target(case str
    [("word") "Word.Application"]
    [("excel") "Excel.Application"]
    [("ppt")"Powerpoint.Application"]
    [else #f]))
  (when target
   (define office (com-create-instance target))
    (define doc(case str
    [("word") (com-get-property office "Documents")]
    [("execl") (com-get-property office "Workbooks")]
    [else  (com-get-property office "Presentations")]))
   (office-to-pdf doc in out)
   (com-invoke office "Quit")
   (com-release office))
  )
;(define word (open-word-auto))
;(define doc (com-get-property word "Documents"))
;(com-set-property! word "DisplayAlerts" 0)
;(word-to-pdf* "D:\\test\\ass.doc" "D:\\test\\ass.doc.pdf.pdf")
;(word-open-file  doc "D:\\test\\ass.doc" #:ReadOnly #t #:OpenAndRepair #f #:NoEncodingDialog #t)