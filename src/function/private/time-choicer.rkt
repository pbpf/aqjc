#lang racket/base
(require racket/gui/base
         racket/class
         racket/format)
(provide time-choice% time-range-choicer%)

(define(make-menu-item mu callback)
  (for ([i (in-range 0 60)])
    (new menu-item%
         [label (~a i #:width  2 #:align 'right #:pad-string "0" )]
         [parent mu]
         [callback (lambda(b e)(callback b e (send mu get-label) (~a i #:width  2 #:align 'right #:pad-string "0" )))])))

(define(make-time-menu pop callback)
  (for([i (in-range 0 24)])
    (define x1(new menu%
                   [label (~a i #:width  2 #:align 'right #:pad-string "0" )]
                   [parent pop]))
    (make-menu-item x1 callback)))

(define time-choice%
  (class combo-field%
    [init-field parent label]
    (super-new [init-value "00:00"]
               [label label]
               [parent parent]
               [choices '()])
    (define main-pop-panel(new popup-menu%[title "小时"]))
    (make-time-menu main-pop-panel (lambda(b e mu i)(send this set-value
                                                          (format "~a:~a" mu i))))
    (define/override(on-popup e)
      (define x1 (send this get-x))
      (define y1 (send this get-y))
      ;(define-values(x2 y2)(send parent client->screen x1 y1))
      ;(define-values(x3 y3)(send win scree->client x2 y2))
    ; (printf "~a,~a\n" x1 x2)
      (send this popup-menu main-pop-panel (+ 0 (- (send this get-width) 0)) (- (send this get-height)200))
      )))
(define time-range%
  (class dialog%
    (init-field callback [parent #f])
    (super-new [parent parent][label ""][width 200])
    (define t1 (new vertical-pane% [parent this]))
    (define t2 (new horizontal-panel% [parent t1]))
    (define start (new time-choice%[label ""][parent t2]))
    (define end (new time-choice%[label ""][parent t2]))
    (define ok (new button% [label "确定"][parent t1]
                    [callback (lambda(b e)(callback b e (send start get-value) (send end get-value))
                                          (send this show #f))]))))
(define time-range-choicer%
  (class combo-field%
    [init-field parent (label #f)]
     (super-new [init-value "00:00-00:00"]
                [label label]
                [parent parent]
                [choices '()])
    (define x-this (send this get-x))
      (define y-this (send this get-y))
      (define-values(x-sc y-sc)(send this client->screen x-this y-this))
     (define main-data-panel(new time-range% [x (+ x-sc (- (send this get-width) 200))]
                                            [y (+ (send this get-height) y-sc)]
                                            [callback (lambda(b e s end)
                                                    (send this set-value
                                                          (format "~a-~a" s end)))]))
    (define/override(on-popup e)
      ;(displayln (send e get-event-type))
      (define x1(send this get-x))
      (define y1(send this get-y))
      (define-values(x2 y2)(send (send this get-parent)client->screen x1 y1))
      ;(printf "x1:~a y1:~a x2:~a y2:~a width:~a height:~a\n" x1 y1  x2 y2 (send this get-width) (send this get-height))
      (send main-data-panel move (+ x2 (- (send this get-width) 200)) (+ (send this get-height) y2))
      (send main-data-panel show #t)
     ; (send this on-subwindow-event this (new mouse-event%	 	[event-type 'left-down]))
      )))

(module+ test
  (define x (new time-range% [callback (lambda(a b c d)(displayln (list c d)))]))
  (send x show #t))
                                     