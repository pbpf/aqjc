#lang racket/base
(require racket/gui/base
         racket/match
         racket/class
        racket-poppler
         net/url
         pict)

(provide path->uri  pdf-viewer-panel%)

(define(path->uri ps)
  (url->string(path->url ps)))

(define pdf-viewer-control%
  (class editor-canvas%
                     [init-field parent [pasteboard (new pasteboard%)]]
                     (field [doc #f]
                            [index -1]
                            [maxindex 0])
                     (super-new [parent parent]
                                [style '(auto-vscroll)]
                                [editor pasteboard])
                     (define/public(load-file file)
                       (set! doc (open-pdf file))
                       (set! maxindex (sub1(pdf-count-pages doc))))
                     (define/public(reset)
                       (send pasteboard erase)
                       (set! doc #f)
                       (set! index -1)
                       (set! maxindex 0))
                     (define/public(show-page n)
                       (when (and doc (<= 0 n maxindex))
                        (send pasteboard erase)
                        (define pict (page->pict  (pdf-page doc n)))
                       (define-values(a b)(send pasteboard get-max-view-size))
                       (define scale-s (* (/ a  (pict-width pict)) 4/5))
                         (define x-start (floor (* 1/10 a)))
                         (send pasteboard insert (make-object image-snip% (pict->bitmap(scale pict scale-s))) x-start 0)
                         (set! index n))
                       index)
                     (define/public(show-page* n)
                       (if(< n 0)
                          (show-page 0)
                          (if(> n maxindex)
                             (show-page maxindex)
                             (show-page n))))
                     (define/public(show-next)
                       (unless (= index maxindex)
                         (show-page (add1 index)))
                       index)
                     (define/public(show-before)
                       (unless (= index 0)
                         (show-page (sub1 index)))
                       index)
                     ))

#|
(define-values(a b)(send this get-client-size))
                         (send this refresh-now	 (lambda(dc)
                                                   (define scale-s (* (/ a  (send pict  get-width)) 4/5))
                                                   
                                                   (define x-start (floor (* 1/8 (send pict  get-width))))
                                                   (send dc set-scale scale-s scale-s)
                                                   (send dc draw-bitmap pict x-start 0)))
                         (set! index n))
|#
(define pdf-viewer-control-plus%
  (class canvas%
                     [init-field parent]
                     (field [doc #f]
                            [view-y-start 0]
                            [view-width #f]
                            [view-height #f]
                            [rec-x1 #f]
                            [rec-x2 #f]
                            [rec-y1 #f]
                            [rec-y2 #f]
                            [x-start #f]
                            [page-scale 1.0]
                            [check-string ""]
                            [text-layout #f]
                            [index -1]
                            [maxindex 0]
                            [bitmap #f])
                     (super-new [parent parent]
                                [paint-callback (lambda(c dc)
                                                  (when bitmap
                                                    
                                                   (send dc draw-bitmap-section	 bitmap x-start 0
                                                         0 view-y-start view-width view-height
                                                         )))]
                                                  
                               [style '(vscroll)]
                                )
                     (define/public(load-file file)
                       (set! doc (open-pdf file))
                       (set! maxindex (sub1(pdf-count-pages doc))))
                     (define/public(reset)
                       (set! doc #f)
                       (set! index -1)
                       (set! maxindex 0)
                       (set! view-y-start 0)
                       (send this set-scroll-pos 'vertical 0)
                       (set! bitmap #f)
                       (send this refresh-now))
                     (define/public(show-page n)
                      ; (printf "~a\n" (send this get-scroll-range 'vertical	))
                       (when (and doc (<= 0 n maxindex))
                        ; (printf "~a\n"  n)
                         (define page (pdf-page doc n))
                         (set! text-layout (page-text-with-layout  page))
                         ;(displayln text-layout)
                        (define pict (page->pict  page))
                         (define w1 (send this get-width))
                         (define scale-s (* (/ w1  (pict-width pict)) 4/5))
                         (define pict2 (scale pict scale-s))
                         (set! view-width (pict-width pict2) )
                         (set! view-height (pict-height pict2))
                         (send this set-scroll-range 'vertical
                               (min 10000(max 10(- (inexact->exact(floor(pict-height pict2)))
                                  (send this get-height)
                                  (send this get-scroll-page 'vertical) -1))))
                         ;(printf "~a\n" scale-s)
                         (set! bitmap (pict->bitmap pict2))
                         (set! x-start (floor (* 1/10  w1)))
                         (set! page-scale scale-s)
                        (send this refresh)
                         (set! index n))
                       index)
                     (define/override(on-char evt)
                       (case (send evt get-key-code)
                         [(wheel-up)(send this set-scroll-pos 'vertical(max 0 (- (send this get-scroll-pos 'vertical) 10)))]
                         [(wheel-down)(send this set-scroll-pos 'vertical(min 1000000(+ (send this get-scroll-pos 'vertical) 10)))]
                         [else #f])
                       (set! view-y-start (send this get-scroll-pos 'vertical ))
                       (send this refresh)
                       ;(send this on-paint)
                       )
                     (define/override(on-scroll evt)
                       (set! view-y-start (send evt get-position ))
                       (send this refresh))
                     (define/override(on-event evt)
                       (when bitmap
                       
                       (cond
                         [(send evt dragging?)
                         ; (define-values(ss sy)(send this get-view-start))
                          (when (and rec-x1 rec-x2)
                          (set! rec-x2 (send evt get-x))
                          (set! rec-y2 (+ view-y-start (send evt get-y))
                                )
                          (define-values(xa1 ya1)(canvas->page rec-x1 rec-y1))
                          (define-values(xa2 ya2)(canvas->page rec-x2 rec-y2))
                          (define page (pdf-page doc index))
                          (define-values( pict str)
                            (for/fold ([pageview (page->pict page)]
                                                 [str ""])
                                    ([bounding-box (in-list text-layout)]
                                     #:when (box-in? bounding-box xa1 ya1 xa2 ya2))
                            (match-define  (list x1 y1 x2 y2) (cadr bounding-box))
 ;; Each match's bounding box ^
                            (values(pin-over pageview x1 y1
                                      (cellophane
                                       (colorize (filled-rectangle (max 0.1 (- x2 x1)) (max 0.1(- y2 y1))) "green")
                                       0.5))
                                   (string-append str ((lambda(x)(if(string=? x "") " " x))(car bounding-box))))
                                   ))
                         (define w1 (send this get-width))
                         (define scale-s (* (/ w1  (pict-width pict)) 4/5))
                         ;(send this set-scroll-range 'vertical	(inexact->exact(floor(* (pict-height pict)scale-s))))
                         (set! bitmap (pict->bitmap(scale pict scale-s)))
                         (set! x-start (floor (* 1/10  w1)))
                         (set! page-scale scale-s)
                        (send this refresh)
                         (set! check-string str)
                            )
                         
                          ]
                         [(send evt button-up? 'right)(define pp (new popup-menu%))
                                                     (new menu-item% [label "复制"][parent pp][callback (lambda(b e)
                                                                                                        (send  the-clipboard set-clipboard-string check-string
                                                                                                               (send e  get-time-stamp)))])
                                                     (send this popup-menu pp  (send evt get-x) (send evt get-y))]
                         [(send evt button-down? 'left)
                          (set! rec-x1 (send evt get-x))
                          (set! rec-y1 (+ view-y-start (send evt get-y)))
                          (show-page index)]
                         [else (void) ])
                       ))
                     (define/private(canvas->page x y)
                       (define x1 (- x x-start))
                       (values (/ x1 page-scale)
                               (/ y page-scale)))
                     (define/private(box-in? box x1 y1 x2 y2)
                       (match-define (list a b c d) (cadr box))
                       (or (and (<= y1 b) (<= d y2))
                           (and (<= x1 c)(<= y1 d y2))
                           (and (<= a x2) (<= y1 b y2))
                           ))
                     ;(define/override(on-scroll s)
                     ;  (printf "~a\n" s))
                     (define/public(show-page* n)
                       (if(< n 0)
                          (show-page 0)
                          (if(> n maxindex)
                             (show-page maxindex)
                             (show-page n))))
                     (define/public(show-next)
                       (unless (= index maxindex)
                         (show-page (add1 index))
                         )
                       index)
                  ; (define/override(on-size w h)
                     ; (printf "~a ~a\n" w h)
                    ;  (send this show-page index))
                     (define/public(show-before)
                       (unless (= index 0)
                         (show-page (sub1 index)))
                       index)
    (send this init-manual-scrollbars			#f 10 10 10 0 0)
                     ))

(define pdf-viewer-panel%
  (class vertical-panel%
    (init-field parent[style '()])
    (super-new [parent parent]
               [style style])
    (define control-bar (new horizontal-panel%
                             [parent this]
                             [stretchable-height #f]
                             [alignment '(center center)]
                             [min-height 10] 
                             ))
    (define view-panel (new   panel%
                             [parent this]
                            ; [style style]
                             [stretchable-height #t]))
    (define pdf-viewer-controler (new pdf-viewer-control-plus%
                                      [parent view-panel]))
    (define/public(load-file f)
      (send pdf-viewer-controler load-file f)
      (send pdf-viewer-controler show-page 0))
    (define/public(reset)
      (send pdf-viewer-controler reset)
      (send skip set-value "0"))
    (define/public(load-file* f)
      (load-file (path->uri f)))
    (new button%	 
   	 	[label "上一页"]	 
   	 	[parent control-bar]
            [callback (lambda(b e)
                        (define t(send pdf-viewer-controler show-before))
                        (send skip set-value (number->string t)))])
    (new button%	 
   	 	[label "下一页"]	 
   	 	[parent control-bar]
            [callback (lambda(b e)
                        (define t(send pdf-viewer-controler show-next))
                        (send skip set-value (number->string t)))])
    (define skip(new text-field% [label "第"]
                     [min-width 100]
                     [stretchable-width #f]
                     [parent control-bar]
                     [init-value "0"]
                     [callback (lambda( b e)
                                 ;(displayln (send e get-event-type))
                                 (when (eq? (send e get-event-type)
                                           'text-field-enter)
                                   (define t(send pdf-viewer-controler show-page* (string->number (send skip get-value))))
                                   (send skip set-value (number->string t))))]))
    (define message(new message%[label "页"]
                        (parent control-bar)))
    ))




(module+ test
  (require framework
           "list-box.rkt"
           )
(define WIDTH  595)
(define HEIGHT 791)



(define m (new frame% [label ""]
               [width 600]
               [height 500]))
  (define nav1-info2 (new panel:vertical-dragable%  [parent m][min-height 566]))
  (define file-list-panel
  (new panel% [parent nav1-info2]))
  (define nav1-file-list
  (new list-box-sort-able%
       [parent file-list-panel]
      ; [multi-choices '(#(1 2) #(4 5))];(query-rows cnn "select filename,date from files where subindex=? order by sortindex" (current-subindex))
       [on-dclick (lambda(x)
                    (void)
                    )]
       [columns '("文件名" "日期")]
       [columns-types '("integer" "integer")]))
(define t(new pdf-viewer-panel% 
              [parent nav1-info2]))
 (send nav1-info2 set-percentages '(17/100 83/100))
(send t load-file (path->uri "D:\\233.pdf"))
(send m show #t))
;(interface->method-names(class->interface pasteboard%))

