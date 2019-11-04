#lang racket/base
(require racket/draw
         racket/file
         ;racket/gui/base
         racket/class)
;a4 pdf
;A4纸的尺寸是210mm×297mm,当你设定的分辨率是72像素/英寸时,A4纸的尺寸的图像的像素是595×842
;默认分辨率为72像素/英寸
;像素是595×842
(define pdf-generator%
  (class  object%
    (super-new)
    (init-field [background-scale 1.0]
                [head-string ""]
                [page-width 595]
                [page-height 842]
                [output (make-temporary-file "rkttmp~a.pdf" #f (find-system-path 'temp-dir ))])
    (field [context (make-hash)])
    (define pdf-dc
      (new pdf-dc%[interactive #f]
               [as-eps #f]
               [output output]
               [width page-width]
               [height page-height]))
    (define/private(get-page x y)
      (define-values(page y1)(quotient/remainder y page-height))
      (values (hash-ref! context page (lambda()(new record-dc% [width page-width][height page-height])))
              x y1))
     (define/private(get-section-page y height);height must less than page-height
      (define-values(page y1)(quotient/remainder y page-height))
      (if(>= (- page-height y1) height)
         (values page  y1 0)
         (values (add1 page)0 1)))
         
    (define/public(draw-text text	 x y [combine? #f] [offset 0] [angle 0])
      (define-values(page x1 y1)(get-page x y))
      (send page draw-text  text x1 y1))
    (define/public(render-pdf)
      (define pages (hash-keys context))
      (unless(null? pages)
        (define maxpage (apply max pages))
        (send pdf-dc start-doc head-string)
        (for([i (in-range 0 (add1 maxpage))])
          (send pdf-dc start-page)
          (when (hash-has-key? context i)
            ((send (hash-ref context i)  get-recorded-procedure)
             pdf-dc)
            )
          (send pdf-dc end-page))
        (send pdf-dc end-doc)))
    ))
#|
(define a(new pdf-generator% [output "d:\\as123.pdf"]))
(send a draw-text "xaaa"  590 0)
;(send a draw-text "xaaa" 200 2000)
(send a render-pdf)
|#