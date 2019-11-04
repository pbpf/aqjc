#lang racket
(require
 scribble/html-properties
           scribble/latex-properties
           scribble/core
           racket/runtime-path
           )
(provide small-style bigger-style imag-path)

(define-runtime-path style-path "styles")
(define-runtime-path imag-path "imag")
(define style-file
  (list (make-css-addition (build-path style-path "Cstyle.css"))
                       (make-tex-addition (build-path style-path "Cstyle.tex")))) 
(define small-style
     (make-style "Cstyle"
                 style-file))
(define bigger-style
     (make-style "Tstyle"
                 style-file))

