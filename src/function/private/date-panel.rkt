#lang racket/base
(require racket/gui/base
         racket/date
         racket/runtime-path
         racket/class
         racket/format
       ;  "configurator/main.rkt"
        ; "tools.rkt"
         )

;(define-sub-configurer  date-panel-config (main-config-init-sub 'date-panel))
(provide date-panel% date-choicer%)

(define(next-date d t)
  (+ d (* 3600 24 t)))

(define-runtime-path pict-path "pict")
(define red-bitmap-map (for/vector ([i (in-range 1 32)])
                     (read-bitmap (build-path pict-path (format "r~a.png" i)))))
(define gray-bitmap-map (for/vector ([i (in-range 1 32)])
                     (read-bitmap (build-path pict-path(format "g~a.png" i)))))
(define white-bitmap-map (for/vector ([i (in-range 1 32)])
                     (read-bitmap (build-path pict-path(format "w~a.png" i)))))

(define(bitmap-ref mp i)
  (vector-ref mp (sub1 i)))

(define(make-month-first-day m y)
  (find-seconds 0 0 0  1 m y))

(define(jmp n)
  (if(= n 0)
     7
     n))
(define(get-last-monday d)
  (- d
      (* 3600 24 (jmp (modulo (- (date-week-day (seconds->date d)) 1) 7)))))

(define(get-first-monday m y)
  (get-last-monday(make-month-first-day m y)))

(define(get-label d m y)
  (define day (date-day d))
  (if(is-date-today? d)
     (bitmap-ref red-bitmap-map day)
     (if(is-date-this-month? d m y)
        (bitmap-ref white-bitmap-map  day)
        (bitmap-ref gray-bitmap-map day))))
(define(is-date-today? d)
  (define d1 (current-date))
  (and (= (date-year d)(date-year d1))
       (= (date-year-day d)(date-year-day d1))))
(define(is-date-this-month? d m y)
  (and (= (date-year d)y)
       (= (date-month d)m)))
(define(next-month m y)
  (if(= m 12)
     (values 1 (+ y 1))
     (values (+ m 1) y)))
(define(before-month m y)
  (if(= m 1)
     (values 12 (- y 1))
     (values (- m 1) y)))
(define day-button%
  (class button%
    [init-field date parent date-parent callback]
    (super-new [parent parent]
               [label (let([d1 (seconds->date date)])
                           (apply get-label d1 (send date-parent get-month)))];(if (is-date-today? d1)(read-bitmap (format "pict/r~a.png" (date-day d1))) (format "~a" (date-day d1)))
               [callback (lambda(b e)(callback b e date))])
    (define/public(move-week n)
      (set! date (next-date date (* 7 n)))
      (define d2 (seconds->date date))
      (send this set-label (apply get-label d2 (send date-parent get-month))))))
;(define-date-panel-config year-name "年")
;(define-date-panel-config month-name "月")
;(define-date-panel-config day-name "日")
(define date-panel%
  (class dialog%
    [init-field [callback (lambda(b e x)(void))] (parent #f)]
    (field [month (date-month (current-date))]
           [year (date-year (current-date))]
            )
    (super-new [label ""]
               [parent parent]
               [style '()])
   (define/public(get-month)
     (list month year))
;(define base-info (new vertical-panel% [parent test]))
    (define date-panel-0 (new horizontal-pane% [parent this]));[alignment '(center center)]
    (new message% [label (parameterize[(date-display-format 'iso-8601)]
                                            (date->string (current-date)))][parent date-panel-0][horiz-margin 30])
    (define year-mg(new message% [label (format "~a" year)][parent date-panel-0]))
    (define year-mg-1(new message% [label "年"][parent date-panel-0]))
    (define month-mg(new message% [label (~a month #:width 2 #:align 'right #:pad-string "0" )][parent date-panel-0][min-width 14]))
    (define month-mg-1(new message% [label "月"][parent date-panel-0]))
    (new button% [label "←"][parent date-panel-0][horiz-margin 20]
         [callback (lambda( b e)(do-move #f))])
    (new button% [label "→"][parent date-panel-0]
         [callback (lambda( b e)(do-move #t))])
    (define date-panel (new horizontal-pane% [parent this]))
    (define date-panel-list (build-list 7 (lambda(x)(new vertical-pane% [parent date-panel]))))
   ; (define beg (get-last-monday(current-date)))
;(get-range-date (get-last-monday (current-date)) 6)
    (define button-table
      (let([date-begin (get-first-monday month  year)])
      (for/list ([i (in-list '("一" "二" "三" "四" "五" "六" "七"))]
          [j (in-list date-panel-list)]
          [k (in-range 0 7)])
      (new message%[label i];(read-bitmap (format "pict/b~a.png" (+ 1 k)))
           [parent j])
      (for/list([p (in-range 0 6)])
        (define dt (next-date date-begin (+ k (* 7 p))))
        (new day-button%[date dt];(read-bitmap (format "pict/~a.png" (date-day(next-date beg (+ k (* 7 p))))))
             [date-parent this]
             [callback (lambda(b e d)(callback b e d)
                                     (send this show #f))]
             [parent j])))))
     (define(do-move next?)
      (define-values(m1 y1)(if next?(next-month month year)(before-month month year)))
       (define nb (get-first-monday m1  y1))
       
       (define to-move (/ (- nb (get-first-monday month year)) 3600 24 7))
       (set! month m1)
       (set! year y1)
      ; (printf "***~a\n" to-move)
       (for ([i (in-list button-table)])
         (for([j (in-list i)])
           (send j move-week to-move)))
       
       (send year-mg set-label (format "~a" year))
       (send month-mg set-label (~a month #:width 2 #:align 'right #:pad-string "0" )))))


(define date-choicer%
  (class combo-field%
    (init-field parent  label [frame #f][min-width #f][callback (lambda(b e)(void))])
    (super-new [init-value (parameterize[(date-display-format 'iso-8601)]
                                            (date->string (current-date)))]
               [label label]
               [parent parent]
               [callback callback]
               [choices '()])
     ;； (define x-this (send this get-x))
      ;(define y-this (send this get-y))
      ;(define-values(x-sc y-sc)(send this client->screen x-this y-this))
    (define main-data-panel (new date-panel% [parent #f]
                                            ;[width 518]
                                           ; [x (+ x-sc (- (send this get-width) 530))]
                                           ; [y (+ (send this get-height) y-sc)]
                                        [callback (lambda(b e x)
                                                    (send this set-value
                                                          (parameterize[(date-display-format 'iso-8601)]
                                                            (date->string (seconds->date x)))))]))
    (define/override(on-popup e)
      ;(displayln (send e get-event-type))
      (define x1(send this get-x))
      (define y1(send this get-y))
      (define-values(x2 y2)(send (send this get-parent) client->screen x1 y1))
      (send main-data-panel move (+ x2 (- (send this get-width) 530)) (+ (send this get-height) y2))
      (send main-data-panel show #t)
     ; (send this on-subwindow-event this (new mouse-event%	 	[event-type 'left-down]))
      )))