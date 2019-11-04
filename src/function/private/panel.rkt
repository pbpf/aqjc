#lang racket/base
(require ;srfi/43
         "table.rkt"
         racket/gui/base
         racket/class
         racket/set
         ;ffi/com
         json)
(provide horizontal-merge-table-panel%
         levels-pane%
         merge-table-panel%)
(define (pos->cons pos)
  (list (pos-row pos) (pos-col pos)))
(define merge-table-panel%
  (class panel%
    
    ;; --------------------------------------------------------------------------
    ;;                           Initializations
    ;; --------------------------------------------------------------------------
    
    ;; parent : (or/c (is-a?/c frame%) (is-a?/c dialog%)
    ;;                (is-a?/c panel%) (is-a?/c pane%))
    (init-field columns rows parent [alignment '(center center)][min-column-width 0][min-row-height 0])
   ; (inherit-field children)
    (field [column-widths (make-vector columns min-column-width)]
           [row-heights (make-vector rows min-row-height)]
           [installed-pos '()]
           [virtual-lines '()])
    ;(init parent)
    (unless (or (is-a? parent frame%) (is-a? parent dialog%)
                (is-a? parent panel%) (is-a? parent pane%))
      (error (format "initialization for table-panel%: expected argument that is an instance of frame%, dialog%, panel%, or pane% for required initialization parent, given ~a"
                     parent)))
    (super-new [parent parent])
     ;-----------------------------------------------------------------------------
     (define/public(item-y p)
      (for/sum([i (in-vector row-heights)]
               [j (in-range 0 (sub1(pos-row (send p get-pos))))])
        i))
    (define/public(load-meta-data metadata)
      ;(define tb (hash-ref metadata 'table))
      (define tb (for/hash ([i (in-list (bytes->jsexpr metadata))])
                   (values (apply pos (car i))(cadr i))))
      ;(write tb)
      (for([i(in-list (send this get-children))]
           #:when (send i meta-edit-able?))
        ;(printf "~a ~a\n" (pos-row(send i get-pos))(pos-col(send i get-pos)))
        (send i set-meta-value (hash-ref tb (send i get-pos)))))
    (define/public(save-meta-data)
      (jsexpr->bytes
       (for/list([i (in-list (send this get-children))]
           #:when (send i meta-edit-able?))
         (list (pos->cons(send i get-pos))
               (send i get-meta-value)))))
     (define/public(fill-word-table word tb)
      (for([i (in-list (send this get-children))])
       ;(printf "~a\n" i)
        (send i write-table word tb (pos->word-pos (send i get-pos)))))
     ;-----------------------------------------------------------------------------
    (define/public(install-pos! pos)
      (set! installed-pos (set-add installed-pos pos)))
     (define/public(set-virtual-line! row)
       (set! virtual-lines (set-add virtual-lines row)))
    (define/public(pos->word-pos post)
      (define t(for/sum([i (in-list virtual-lines)]
               #:when (< i (pos-row post)))
                 1))
      (pos (- (pos-row post) t ) (pos-col post)))
;--------------------------------insert-row-after-copy-bef---------------------------------------
    (define/public(insert-row-after-copy-bef row)
      (send this begin-container-sequence)
      (set! row-heights(insert-vector-after row-heights rows row))
      (set! rows (add1 rows))
      (for ([i (in-list (send this get-children))])
        (send i after-insert-row row))
      (set! virtual-lines (map (lambda(t)(if(< row t)(add1 t) t))virtual-lines))
      (for-each (lambda(x)(send x after-insert-row row))installed-pos)
      (send this end-container-sequence))
;-------------------------------------------------------------------------------------------------
    (define/public(delete-row row)
      (send this begin-container-sequence)
      (for([i (in-list (send this get-children))])
        (send i after-delete-row row))
      (set! row-heights (delete-vector row-heights rows row))
      (for-each (lambda(x)(send x after-delete-row row))installed-pos)
      (set! rows (sub1 rows))
      (send this  end-container-sequence)
      )
    
    ;; --------------------------------------------------------------------------
    (define(get-children)
      (send this get-children))
    (define/override (container-size info)
      ;(displayln info)
      (for([i(in-range 0 columns)])
        ;(displayln (computer-column-widths info (get-children) column-widths i))
        (vector-set! column-widths i (computer-column-widths info (get-children) column-widths i)))
     ; (printf "in ~a ~a\n" info row-heights)
      (for([i(in-range 0 rows)])
        ;(displayln (computer-column-widths info (get-children) column-widths i))
        (vector-set! row-heights i (computer-row-heights info (get-children) row-heights i)))
     ; (printf "out ~a\n" row-heights)
     ;(printf "~a ~a\n" column-widths row-heights)
      (values (sum-vector column-widths)
              (sum-vector row-heights)))
    
    ;; --------------------------------------------------------------------------
    ;;                           Child Placement
    ;; --------------------------------------------------------------------------
    (define (place-child column-widths-sum row-heights-sum child-info pos merge)
     ;(displayln row-heights-sum)
      (let([x(merge-column-x column-widths-sum merge)]
           [y(merge-row-y row-heights-sum merge pos)]
           [w(merge-column-size column-widths-sum merge)]
           [h(merge-row-size row-heights-sum merge pos)]
           [child-w (car child-info)]
           [child-h (cadr child-info)]
           [child-stretchable-width (caddr child-info)]
           [child-stretchable-height (cadddr child-info)])
      ; (printf "~a ~a ~a ~a\n" x y w h)
        (if child-stretchable-width
            (if child-stretchable-height
                (list x y w h)
                (case (cadr alignment)
                  [(top)(list x y w child-h)]
                  [(center)(list x (+ y (floor(/ (- h child-h) 2))) w child-h)]
                  [else (list x (+ y(- h child-h)) w child-h)]))
            (if child-stretchable-height
                (case (car alignment)
                  [(left)(list x y child-w h)]
                  [(center)(list x (+ y (floor(/ (- h child-h) 2))) w child-h)]
                  [else (list (+ x (floor(/ (- w child-w) 2))) y child-w h)])
                 (case alignment
                  [((left top))(list x y child-w child-h)]
                  [((left center))(list x (+ y (floor(/ (- h child-h) 2))) child-w child-h)]
                  [((left bottom))(list x (+ y (- h child-h)) child-w child-h)]
                  [((center top))(list (+ x (floor(/ (- w child-w) 2))) y child-w child-h)]
                  [((center center))(list (+ x (floor(/ (- w child-w) 2))) (+ y (floor(/ (- h child-h) 2))) child-w child-h)]
                  [((center bottom))(list (+ x (floor(/ (- w child-w) 2))) (+ y (- h child-h)) child-w child-h)]
                  [((right top))(list (+ x (- w child-w)) y  child-w child-h)]
                  [((right center))(list (+ x (- w child-w)) (+ y (floor(/ (- h child-h) 2)))  child-w child-h)]
                  [else (list (+ x (- w child-w)) (+ y (- h child-h))  child-w child-h)])))))
           

    (define/override (place-children info width height)
     ; (displayln 1)
     ; (displayln 2)
      ;(displayln info)
      (let([column-widths-sum (vector-fold-sum column-widths columns)]
           [row-heights-sum   (vector-fold-sum row-heights rows)])
        ;(printf "sum ~a ~a\n" column-widths-sum row-heights-sum)
          (let([column-sum-this (scale-vector column-widths-sum (/ width (vector-ref column-widths-sum columns)))]
               [row-sum-this (scale-vector row-heights-sum (/ height (vector-ref row-heights-sum  rows)))])
          (for/list ((child-info (in-list info))
                     (child (in-list (get-children))))
              (place-child
               column-sum-this
               row-sum-this
               child-info
               (send child get-pos)
               (send child get-merge))))))
    
    )
  )
(define horizontal-merge-table-panel%;;;;允许截断打印
  (class panel%
    
    ;; --------------------------------------------------------------------------
    ;;                           Initializations
    ;; --------------------------------------------------------------------------
    
    ;; parent : (or/c (is-a?/c frame%) (is-a?/c dialog%)
    ;;                (is-a?/c panel%) (is-a?/c pane%))
    (init-field columns rows parent [alignment '(center center)])
   ; (inherit-field children)
    (field [column-widths (make-vector columns 0)]
           [row-heights (make-vector rows 0)]
           [installed-pos '()]
           [variable-row-mangers (make-hash)]
           [virtual-lines '()])
    ;(init parent)
    (unless (or (is-a? parent frame%) (is-a? parent dialog%)
                (is-a? parent panel%) (is-a? parent pane%))
      (error (format "initialization for table-panel%: expected argument that is an instance of frame%, dialog%, panel%, or pane% for required initialization parent, given ~a"
                     parent)))
    (super-new [parent parent])
    (define/public(item-y p)
      (for/sum([i (in-vector row-heights)]
               [j (in-range 0 (sub1(pos-row (send p get-pos))))])
        i))
     ;-----------------------------------
    (define/public(fill-word-table word tb)
      (for([i (in-list (send this get-children))])
       ; (printf "~a ~a\n" (send i get-pos) (pos->word-pos (send i get-pos)))
        (send i write-table word tb (pos->word-pos (send i get-pos)))))
    ;--------------------------------------
    (define/public(load-meta-data metadata)
      (define metadata-tb (bytes->jsexpr metadata))
      (define row-mangers-tb (for/hash ([i (in-list (hash-ref metadata-tb 'row-mangers))])
                               (values (car i)(cadr i))))
      (define item-tb (hash-ref metadata-tb 'item))
      (define tb (for/hash ([i (in-list (hash-ref metadata-tb 'item))])
                   (values (apply pos (car i))(cadr i))))
      (for([(k v)(in-hash variable-row-mangers)])
        (send v set-meta-data (hash-ref row-mangers-tb k)))
      ;(write tb)
      (for([i(in-list (send this get-children))]
           #:when (send i meta-edit-able?))
        ;(printf "~a ~a\n" (pos-row(send i get-pos))(pos-col(send i get-pos)))
        (send i set-meta-value (hash-ref tb (send i get-pos)))))
    (define/public(save-meta-data)
      (jsexpr->bytes
       (hash 'row-mangers (for/list([(k v)(in-hash variable-row-mangers)])
                            (list k (send v get-meta-data)))
             'item
       (for/list([i (in-list (send this get-children))]
           #:when (send i meta-edit-able?))
         (list (pos->cons(send i get-pos))
               (send i get-meta-value))))))
    ;-----------------------------------
    (define/public(install-variable-row-manger index m)
      (cond
        [(hash-has-key? variable-row-mangers index)
         (error 'install-variable-row-manger "manger key ~a already in use" index)]
        [(hash-set! variable-row-mangers index m)]))
    ;------------------------------
     (define/public(install-pos! pos1)
      (set! installed-pos (set-add installed-pos pos1)))
     (define/public(set-virtual-line! row)
       (set! virtual-lines (set-add virtual-lines row)))
    (define/public(pos->word-pos pos1)
      (define t(for/sum([i (in-list virtual-lines)]
               #:when (< i (pos-row pos1)))
                 1))
      (pos (- (pos-row pos1) t ) (pos-col pos1)))
;--------------------------------insert-row-after-copy-bef---------------------------------------
    (define/public(insert-row-after-copy-bef row)
      (send this begin-container-sequence)
      (set! row-heights(insert-vector-after row-heights rows row))
      (set! rows (add1 rows))
      (for ([i (in-list (send this get-children))])
        (send i after-insert-row row))
      (set! virtual-lines (map (lambda(t)(if(< row t)(add1 t) t))virtual-lines))
      (for-each (lambda(x)(send x after-insert-row row))installed-pos)
      (send this end-container-sequence))
;-------------------------------------------------------------------------------------------------
    (define/public(delete-row row)
      (send this begin-container-sequence)
      (for([i (in-list (send this get-children))])
        (send i after-delete-row row))
      (set! row-heights (delete-vector row-heights rows row))
      (for-each (lambda(x)(send x after-delete-row row))installed-pos)
      (set! rows (sub1 rows))
     (send this  end-container-sequence)
      )
    
    ;; --------------------------------------------------------------------------
    (define(get-children)
      (send this get-children))
    (define/override (container-size info)
      ;(displayln 1)
      (for([i(in-range 0 columns)])
        ;(displayln (computer-column-widths info (get-children) column-widths i))
        (vector-set! column-widths i (computer-column-widths info (get-children) column-widths i)))
      (for([i (in-list (get-children))]
           [j (in-list info)])
        (define pos (send i get-pos))
        (vector-set! row-heights (sub1(pos-row pos)) (max (vector-ref row-heights (sub1(pos-row pos))) (cadr j))))
     ; (printf "~a ~a\n" column-widths row-heights)
      (values (sum-vector column-widths)
              (sum-vector row-heights)))
    
    ;; --------------------------------------------------------------------------
    ;;                           Child Placement
    ;; --------------------------------------------------------------------------
    (define (place-child column-widths-sum row-heights-sum child-info pos merge)
    ; (displayln row-heights-sum)
      (let([x(merge-column-x column-widths-sum merge)]
           [y(vector-ref row-heights-sum (sub1(pos-row pos)))]
           [w(merge-column-size column-widths-sum merge)]
           [h(-(vector-ref row-heights-sum (pos-row pos))(vector-ref row-heights-sum (sub1(pos-row pos))))]
           [child-w (car child-info)]
           [child-h (cadr child-info)]
           [child-stretchable-width (caddr child-info)]
           [child-stretchable-height (cadddr child-info)])
      ; (printf "~a ~a ~a ~a\n" x y w h)
        (if child-stretchable-width
            (if child-stretchable-height
                (list x y w h)
                (case (cadr alignment)
                  [(top)(list x y w child-h)]
                  [(center)(list x (+ y (floor(/ (- h child-h) 2))) w child-h)]
                  [else (list x (+ y(- h child-h)) w child-h)]))
            (if child-stretchable-height
                (case (car alignment)
                  [(left)(list x y child-w h)]
                  [(center)(list x (+ y (floor(/ (- h child-h) 2))) w child-h)]
                  [else (list (+ x (floor(/ (- w child-w) 2))) y child-w h)])
                 (case alignment
                  [((left top))(list x y child-w child-h)]
                  [((left center))(list x (+ y (floor(/ (- h child-h) 2))) child-w child-h)]
                  [((left bottom))(list x (+ y (- h child-h)) child-w child-h)]
                  [((center top))(list (+ x (floor(/ (- w child-w) 2))) y child-w child-h)]
                  [((center center))(list (+ x (floor(/ (- w child-w) 2))) (+ y (floor(/ (- h child-h) 2))) child-w child-h)]
                  [((center bottom))(list (+ x (floor(/ (- w child-w) 2))) (+ y (- h child-h)) child-w child-h)]
                  [((right top))(list (+ x (- w child-w)) y  child-w child-h)]
                  [((right center))(list (+ x (- w child-w)) (+ y (floor(/ (- h child-h) 2)))  child-w child-h)]
                  [else (list (+ x (- w child-w)) (+ y (- h child-h))  child-w child-h)])))))
           

    (define/override (place-children info width height)
      ;(displayln 2)
     ; (displayln 2)
      ;(displayln info)
      (let([column-widths-sum (vector-fold-sum column-widths columns)]
           [row-heights-sum   (vector-fold-sum row-heights rows)])
       ; (printf "sum ~a ~a\n" column-widths-sum row-heights-sum)
          (let([column-sum-this (scale-vector column-widths-sum (/ width (vector-ref column-widths-sum columns)))]
               [row-sum-this (scale-vector row-heights-sum (/ height (vector-ref row-heights-sum  rows)))])
          (for/list ((child-info (in-list info))
                     (child (in-list (get-children))))
              (place-child
               column-sum-this
               row-sum-this
               child-info
               (send child get-pos)
               (send child get-merge))))))
    
    )
  )


(define levels-pane%;;;;
  (class panel%
    
    ;; --------------------------------------------------------------------------
    ;;                           Initializations
    ;; --------------------------------------------------------------------------
    
    ;; parent : (or/c (is-a?/c frame%) (is-a?/c dialog%)
    ;;                (is-a?/c panel%) (is-a?/c pane%))
    (init-field  parent [alignment '(center center)])
   ; (inherit-field children)
    ;(init parent)
    (unless (or (is-a? parent frame%) (is-a? parent dialog%)
                (is-a? parent panel%) (is-a? parent pane%))
      (error (format "initialization for table-panel%: expected argument that is an instance of frame%, dialog%, panel%, or pane% for required initialization parent, given ~a"
                     parent)))
    (super-new [parent parent])
 
    
    ;; --------------------------------------------------------------------------
#|
    (define/override (container-size info)
      ;(displayln 1)
      (for([i(in-range 0 columns)])
        ;(displayln (computer-column-widths info (get-children) column-widths i))
        (vector-set! column-widths i (computer-column-widths info (get-children) column-widths i)))
      (for([i (in-list (get-children))]
           [j (in-list info)])
        (define pos (send i get-pos))
        (vector-set! row-heights (sub1(pos-row pos)) (max (vector-ref row-heights (sub1(pos-row pos))) (cadr j))))
     ; (printf "~a ~a\n" column-widths row-heights)
      (values (sum-vector column-widths)
              (sum-vector row-heights)))
|#
    
    ;; --------------------------------------------------------------------------
    ;;                           Child Placement
    ;; --------------------------------------------------------------------------
    (define (place-child width height child-width child-height child-width-stretchable? child-height-stretchable?)
      (define w (if child-width-stretchable? width child-width))
      (define h (if child-height-stretchable? height child-height))
      (define x (case (car alignment)
                  [(left)0]
                  [(center)(quotient (- width w) 2)]
                  [else (- width w)]))
      (define y (case (cadr alignment)
                  [(top)0]
                  [(center)(quotient (- height h) 2)]
                  [else (- height h)]))
      (list x y w h))
    (define/override (place-children info width height)
          (for/list ((child-info (in-list info)))
              (apply place-child  width height  child-info)))
    
    )
  )
       