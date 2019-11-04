#lang racket/base
(require framework
         racket/list
         racket/set
         racket/gui/base
         racket/class
        "preferences.rkt"
       ; "odbc.rkt"
         "tools.rkt"
         "../tools.rkt")

(provide list-box-sort-able%)

(define (format-list lst type-list)
  (for/list ([i (in-list lst)]
             [j (in-list type-list)])
  ((type->format j) i)))
(define(get-max-width lst)
  (for/fold([maxlen 0])
           ([i (in-list lst)])
    (define-values (a b)(get-window-text-extent i view-control-font))	 
    (values (max maxlen a))))
;
(define(sort-multi-choices multi-choices column column-types)
  (sort multi-choices
        (type->order (list-ref column-types column))
        #:key (lambda(x)(vector-ref x column))))

(define (list-insert lst n v)
  (let-values([(a b)(split-at lst n)])
    (append a (list v) b)))

(define(swap lst i j)
  (define t (list-ref lst i))
  (list-set
   (list-set lst i (list-ref lst j))
   j t))

(define(transpose-format s columns-types)
  (for/list ([i (in-naturals 1)]
             [t (in-list columns-types)])
    (for/list([j (in-list s)])
      ((type->format t)(vector-ref  j i)))))

(define(transpose-format* s columns-types)
     (transpose-format s columns-types))
(define list-box-sort-able%
  (class list-box%
    (init-field  columns
                 columns-types
                 parent
                 [on-dclick  #f]
                 [font view-control-font]
                 [on-click void]
                 [min-width  #f]
                 [min-height #f]
                 [reload (lambda(x)(void))])
    (field [order-multi-choices '()]
           [unsort-colum 0]
           [unsort-colum-lst '()]
           [last-sort-column -1])
    (define/public(reload* x)
      (reload x))
    (define/public(reset)
      (set! order-multi-choices '())
      (reset-order-multi-choices))
    (define/public(reset-order-multi-choices)
      (define newlist2 (transpose-format* order-multi-choices columns-types))
      (set! unsort-colum-lst (build-list (length order-multi-choices) (lambda(x)(number->string (add1 x)))))
     ; (displayln newlist2)
      (send/apply this set unsort-colum-lst newlist2)
      (for([i(in-naturals 1 )]
           [j(in-list newlist2)])
        ;(displayln (get-max-width j))
        ;(define size (inexact->exact(floor(send font get-size))))
        (define t (get-max-width j))
        (send this set-column-width i (+ t 30) (min t 10) 1000))
      (for([i (in-list order-multi-choices)]
           [j (in-naturals 0)])
        (send this set-data j (vector-ref i 0))
        ))
    (define/public(load-multi-choices s)
      (set! order-multi-choices s)
      (reset-order-multi-choices))
   (define/public(get-max-index)
     (- (length order-multi-choices)1))
     (define/public(get-selection-files-id);id
      ;(write order-multi-choices)
      (for/list ([i (in-list (send this get-selections))])
        (vector-ref (list-ref order-multi-choices i)0)))
    (define/public(get-selection-file selection)
     (vector-ref (list-ref order-multi-choices selection)0))
    (define/public(move-down-select)
      (define sels(send this get-selections))
      (when (= 1 (length sels))
        (unless (=  (get-max-index) (car sels))
          (define to  (get-selection-file (add1(car sels))))
          (files:swapindex (get-cnn) (get-current-subindex)
                           (get-selection-file (car sels))
                           to)
          (reload* (get-current-subindex))
          (send this select (add1(car sels)))
          )))
    (define/public(move-up-select)
      (define sels(send this get-selections))
      (when (= 1 (length sels))
        (unless (=  0 (car sels))
          (define to  (get-selection-file (sub1(car sels))))
          (files:swapindex (get-cnn) (get-current-subindex)
                           (get-selection-file (car sels))
                           to)
          (reload* (get-current-subindex))
          (send this select (sub1(car sels)))
          )))
    #|
   (define/public(up-move index)
     (unless (= index 0)
       (set! order-multi-choices (swap order-multi-choices index (- index 1)))
     (reset-order-multi-choices)))
    (define/public(up-down index)
     (unless (= index (- (length order-multi-choices) 1))
       (set! order-multi-choices (swap order-multi-choices index (+ index 1)))
       (reset-order-multi-choices)))
    (define/public(insert-line n v)
      (set! order-multi-choices (list-insert order-multi-choices n v))
      ;(displayln 'b)
      (reset-order-multi-choices))
    (define/public(get-selection-files-id);id
      ;(write order-multi-choices)
      (for/list ([i (in-list (send this get-selections))])
        (vector-ref (list-ref order-multi-choices i)0)))
    ;or get-data n

    (define/public(get-selection-file selection)
     (vector-ref (list-ref order-multi-choices selection)0))

    (define/public(delect-lines lst)
      (set! order-multi-choices
            (for/list ([i (in-list order-multi-choices)]
                 [j (in-naturals 0)]
                 #:unless (set-member? lst j))
                i))
      (reset-order-multi-choices))
    (define/public(delect-selection-lines)
      (delect-lines (send this get-selections)))
           (super-new
            [label #f]
            [choices unsort-colum-lst]
            [parent parent]
            [min-width min-width]
            [min-height min-height]
            [style(list  'multiple 'extended  'vertical-label  'column-headers 'clickable-headers 'reorderable-headers 'variable-columns
                          )]
            [columns (cons "序号" columns)]
            [callback (lambda(b e)
                     (case (send e get-event-type)
                       [(list-box)(on-click (map (lambda(x)(list-ref order-multi-choices x)) (send this get-selections)))]
                       [(list-box-dclick)(let([c (send this get-selections)])(unless (null? c)
                                           (on-dclick (list-ref order-multi-choices (car c)))))]))])
|#
    (super-new
            [label #f]
            [choices unsort-colum-lst]
            [parent parent]
            [font font]
            [min-width min-width]
            [min-height min-height]
            [style(list  'multiple 'extended  'vertical-label  'column-headers 'clickable-headers 'reorderable-headers 'variable-columns
                          )]
            [columns (cons "序号" columns)]
            [callback (lambda(b e)
                      ;  (displayln (send e get-event-type))
                     (case (send e get-event-type)
                       
                       [(list-box)(on-click (map (lambda(x)(list-ref order-multi-choices x)) (send this get-selections)))]
                       [(list-box-dclick)(let([c (send this get-selections)])(unless (null? c)
                                           (on-dclick (list-ref order-multi-choices (car c)))))]))])
    ))