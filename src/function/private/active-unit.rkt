#lang racket/base
(require "label-bitmap.rkt"
         racket/gui/base
         racket/class)

(provide active-button-mixin% active-panel-mixin%)

(define active-button<%> (interface()active))
(define active-panel<%> (interface()active-child))
(define active-button-mixin%
  (mixin (window<%> control<%>)(active-button<%>)
    (init-field colors label min-width min-height [active? #f][callback (lambda(b e)(void))])
    (define active-bitmap (make-label-bitmap label min-width min-height(cadr colors)))
    (define inactive-bitmap (make-label-bitmap label  min-width min-height(car colors)))
    (define/public(active yes?)
      (unless (eq? yes? active?)
       ; (displayln "ehho")
          (send this set-label (if yes? active-bitmap inactive-bitmap))
          (set! active? yes?)
          (when yes?
            (send (send this get-parent) active-child this))))
     (super-new [label (if active? active-bitmap inactive-bitmap)]
               [callback (lambda(b e)(callback b e)(active #t))]
               [min-width min-width]
               [min-height min-height])
    ))

(define active-panel-mixin%
  (mixin (area<%>) ()
    (super-new)
    (define/public(active-child son)
      (for([i (in-list (send this get-children))]
           #:unless (object=? i son))
        (send i active #f)))))
              

#|
(define test (new frame% [label "a"]))
(define p% (active-panel-mixin% horizontal-pane%))
(define p (new p% [parent test]))
(define ab% (active-button-mixin% button%))
(define x (new ab% [label "aaa"][colors '("white" "gray")][min-width 20][min-height 20]
               [parent p]))
(define y (new ab% [label "bbb"][colors '("white" "gray")][min-width 20][min-height 20]
               [parent p]))
|#