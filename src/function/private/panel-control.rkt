#lang racket/base
(require racket/gui/base
         racket/class
         racket/set)

(provide panel-single-mixin%
         panel-index-mixin%
         tab-panel-plus-mixin%)

(define panel-single<%> (interface()active-child))
(define panel-index<%> (interface()get-index))
(define panel-single-mixin%
    (mixin (area-container<%>) (panel-single<%>)
      (inherit get-alignment change-children)
      (define/override(add-child c)
        (displayln c)
        
        (displayln "n")
        (super add-child c)
        (send c show #f))
      (define/override (after-new-child c)
        ;(displayln "hello")
        (unless (is-a? c window<%>)
          
          ;; would like to remove the child here, waiting on a PR submitted
          ;; about change-children during after-new-child
          (change-children
           (λ (l)
             (remq c l)))
          
          (error 'single-mixin::after-new-child
                 "all children must implement window<%>, got ~e"
                 c))
        (if current-active-child
            (send c show #f)
            (set! current-active-child c)))
      [define/override (container-size l)
        (if (null? l)
            (values 0 0)
            (values (apply max (map car l)) (apply max (map cadr l))))]
      [define/override (place-children l width height)
        (let-values ([(h-align-spec v-align-spec) (get-alignment)])
          (let ([align
                 (λ (total-size spec item-size)
                   (floor
                    (case spec
                      [(center) (- (/ total-size 2) (/ item-size 2))]
                      [(left top) 0]
                      [(right bottom) (- total-size item-size)]
                      [else (error 'place-children
                                   "alignment spec is unknown ~a\n" spec)])))])
            (map (λ (l) 
                   (let*-values ([(min-width min-height h-stretch? v-stretch?)
                                  (apply values l)]
                                 [(x this-width)
                                  (if h-stretch?
                                      (values 0 width)
                                      (values (align width h-align-spec min-width)
                                              min-width))]
                                 [(y this-height)
                                  (if v-stretch?
                                      (values 0 height)
                                      (values (align height v-align-spec min-height)
                                              min-height))])
                     (list x y this-width this-height)))
                 l)))]
      
      (inherit get-children begin-container-sequence end-container-sequence)
      [define current-active-child #f]
      (define/public active-child
        (case-lambda
          [() current-active-child]
          [(x) 
           (unless (memq x (get-children))
             (error 'active-child "got a panel that is not a child: ~e" x))
           (unless (eq? x current-active-child)
             (begin-container-sequence)
             (for-each (λ (x) (send x show #f))
                       (get-children))
             (set! current-active-child x)
             (send current-active-child show #t)
             (end-container-sequence))]))
      (super-instantiate ())))
(define panel-single% (panel-single-mixin% panel%))
(define panel-index-mixin%
  (mixin (area-container<%>)(panel-index<%>)
    (init-field index)
    (define/public(get-index)
      index)
    (super-new)))
(define tab-panel-plus-mixin%
  (mixin(area-container<%>) ()
    (init-field [callback (lambda(b e)(void))]
                [child-table (make-hash)]
                )
    (define init? #f)
    (super-new [callback (lambda(b e)(callback b e)
                           (send main-box active-child (hash-ref child-table (send this get-selection))))])
    (define main-box (new panel-single% [parent this]))
    
    (define/override(after-new-child c)
      (when init?
      (send c reparent main-box)
        
      (hash-set! child-table (length (hash-keys child-table)) c))
      (set! init? #t)
    )))


 (define m(new frame% [label  "a"]
                       [min-width 500]
                       [min-height 300]
                       ))
(define b (new (tab-panel-plus-mixin% tab-panel%)[parent m][choices '("a" "b")]))
(define p1 (new  panel%[parent b]))
(define p2 (new panel%[parent b]))

(new message% [label "hello"][parent p1])
(new message% [label "hello2"][parent p2])
#|
  (define main-frame1(new frame% [label  "a"]
                       [min-width 500]
                       [min-height 300]
                       ))
 (define h1 (new   	horizontal-panel%[parent main-frame1]))
  (define left (new (panel-control-mixin panel%)[parent h1]))
  (define right (new panel%[parent h1]))
 (define l2 (new panel%[parent left]))
  (define l1 (new vertical-panel%[parent left]))
 
    (define r1 (new vertical-panel%[parent right]))
  (new button%[label "a"][parent l1])
  (new button%[label "b"][parent l1])
  (new button%[label "c"][parent l1])
   (new button%[label "a2"][parent l2])
 (new button%[label "x"][parent r1])
  (new button%[label "y"][parent r1])
  (new button%[label "z"][parent r1])
  (send main-frame1 show #t)
|#
(send m show #t)