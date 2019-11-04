#lang racket/base
(require ffi/com
          racket/generator)
(provide table-ref cell-insert-pict! cell-insert-pict-auto! cell-insert-text! open-word-auto file-get-tables item-ref attr-ref doc-path
         open-doc
         close-file-without-save-change
         create-new-instance)
;get all table from word
;word-to-csv

(define(attrs t)
  (com-get-properties t))
(define(attrs-writeable t)
  (com-set-properties t))
(define(methods t)
  (com-methods t))
(define(show-obj t)
  (list (list "methods" (map (lambda(x)(list x (com-method-type t x)))
                             (methods t)))
        (list "attrs" (map (lambda(x)(list x 
                                                     (com-get-property-type t x)))(attrs t)))
        (list "attrs-writeable" (map (lambda(x)(list x 
                                                     (com-set-property-type t x)))
                                                     (attrs-writeable t)))))
(define(call obj . t)
  (apply com-invoke obj t))
(define(attr-ref t . attrname)
  (apply com-get-property t attrname))
(define(attr-set! t name value)
  (com-set-property! t  name value))
(define(file-get-tables file)
 (com-get-property file "Tables"))

(define(item-count x)
  (com-get-property x "count"))

(define(item-ref x i)
  (com-invoke x "Item" i))

(define(cell-row-count word c)
  (cell-get-select! c)
  (define s (attr-ref word "Selection"))
  (call s "SelectRow")
  (attr-ref s "Rows" "Count"))
(define(cell-col-count word c)
  (cell-get-select! c)
  (define s (attr-ref word "Selection"))
  (call s "SelectColumn")
  (attr-ref s "Columns" "Count"))
(define(table-count tfield)
  (item-count tfield))

(define(doc-path doc)
  (path->string(build-path (attr-ref doc "Path")
              (attr-ref doc "Name"))))
(define(table-sharp t)
  (values (item-count(table-rows t))
          (item-count(table-cols t))))

(define(table-rows t)
  (com-get-property t "Rows"))
(define(table-cols t)
  (com-get-property t "Columns"))
(define(obj-cells t)
  (com-get-property t "Cells"))


(define(table-ref t i j)
  (com-invoke t "cell" i j))

(define(in-cells tb)
  (in-generator (let loop ([cell (call tb "Cell" 1 1)])
                  (when cell
                    (yield cell)
                    (loop (attr-ref cell "Next"))))))
(define(cell-text c)
  (com-get-property(com-get-property c "Range") "Text"))

(define(get-bookmarks file)
  (com-get-property file "BookMarks"))
(define(BookMarks-ref b s)
  (item-ref b (box s)))
(define(cell-index cell)
  (values (attr-ref cell  "RowIndex")
           (attr-ref cell  "ColumnIndex")))

(define(cell-get-select! cell)
  (call cell "select"))
(define(cell-insert-pict! word cell  pict-path);-->pict
 ; (cell-get-select! cell)
  (call (attr-ref word "Selection" "InlineShapes")
        "AddPicture" pict-path (box #f) (box #t) (box(com-get-property cell "Range"))));ilename, ref linkToFile, refsaveWithDocument, ref range);
(define(resize-pict pict w h)
  (com-set-property! pict "Width" w)
  (com-set-property! pict "Height" h))
(define(cell-insert-pict!* word cell pict-path w h)
  (resize-pict(cell-insert-pict! word cell  pict-path)
              w h))
(define(cell-insert-pict-auto! word cell  pict-path)
  (define-values(w h)(cell-size cell))
  (define t(cell-insert-pict! word cell  pict-path))
  (when (< w 99990)
    (com-set-property! t "Width" w))
  (when (< h 99990)
    (com-set-property! t "Height" h)))
(define(cell-insert-text! c t)
  (com-set-property!(com-get-property c "Range") "Text" t))
(define(cell-size c)
  (values (attr-ref c "Width")
          (attr-ref c "Height")))
(define(close-file-without-save-change file)
  (call file "Close" (box 0)))
(define(saveas file path-string)
  (call file "SaveAs" (box path-string)))

(define(format-table t)
  (define-values(r c)(table-sharp t))
  (for/fold([sr ""])
             ([i (in-range 1 (add1 r))])
     (string-append sr 
     (for/fold ([s (cell-text (table-ref t i 1))])
               ([j (in-range 2 (add1 c))])
       (printf "~a,~a,~a\n" i j (cell-text (table-ref t i j)))
      (string-append s "," (cell-text (table-ref t i j))))
     "\n")))

(define(save-table-to tfield dir)
;  (define word (com-create-instance "Word.Application"))
  ;(define doc (com-get-property word "Documents"))
  (for ([i (in-range 1 (add1(table-count tfield)))])
    (write-string (format-table(com-invoke tfield "Item" i))
                  (build-path dir (format "~a.csv" i)))))

(define(create-new-instance)
  (com-create-instance "Word.Application"))

(define(open-word-auto)
  (with-handlers ([exn:fail? (lambda(x)(create-new-instance))])
    (com-get-active-object "Word.Application")))

(define(open-doc word path)
    (define path* (if(string? path) (path->string (string->path path)) (path->string path)))
    (define docs (attr-ref word "Documents"))
    (define doc* (for/first ([i (in-range 1 (add1 (attr-ref docs "Count")))]
                 #:when (string=? path* (doc-path (item-ref docs (box i)))))
                  (item-ref docs (box i))))
    (cond
      [doc* (com-invoke doc* "Activate") doc*]
      [else (com-invoke docs "open" path* com-omit #t)]))

(define(save-word-to-csv f dir)
  (define word (open-word-auto))
  (define doc (com-get-property word "Documents"))
  (define file (com-invoke doc "open" f  com-omit #t #t))
  (save-table-to (file-get-tables file) dir)
  (com-invoke file "Close")
  (com-invoke word "Quit")
  (com-release word))

(define(cell-shape c)
  (cell-get-select! c)
  (define r (attr-ref c "Range"))
  (values (attr-ref r "Columns" "Count")
          (attr-ref r "Rows" "Count")))
;COM object exception during "Item"
;Description: 无法访问此集合中单独的行,因为表格有纵向合并的单元格。 (800a1767)
#|
(define word (open-word-auto))
(define doc (com-get-property word "Documents"))
(define file (com-invoke doc "open" "D:\\gc5.docx" com-omit #t))
(define tbs (file-get-tables file))
(define tb1 (item-ref tbs 1))
(define t11 (table-ref tb1 1 1))
(cell-insert-pict word t11 "t.jpg")
cell-row-merge
 (call t "Select")
(call (attr-ref word "Selection" "Cells")"Split" (box 3) (box 1))
|#
