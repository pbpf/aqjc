#lang racket/base
(require racket/draw
         racket/class)
(provide make-label-bitmap)

(define (make-label-bitmap  str w h background)
  ;(define target (make-object bitmap% w h)) ; A 30x30 bitmap
  (define dc (new bitmap-dc% [bitmap (make-bitmap 1 1)]))
  (define the-font (make-font #:size 12 #:family 'default
                              #:style 'normal  
                            #:weight 'light #:smoothing 'smoothed ))
  (send dc set-font the-font)
  (define-values (tw th)
  (let-values ([(tw th ta td)
                (send dc get-text-extent str the-font)])
    (values (inexact->exact (ceiling tw))
            (inexact->exact (ceiling th)))))
  (define width (max w tw))
  (define height (max h th))
  (send dc set-bitmap (make-bitmap width height))
  (send dc set-smoothing 'aligned)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush background 'solid)
  (send dc draw-rectangle 0 0 width width)
  (send dc draw-text str (/ (- width tw) 2) (/ (- height th) 2))
  (send dc get-bitmap))
;(make-label-bitmap "你好" 40 20 "gray")