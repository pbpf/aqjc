#lang racket/base
(require "nav3-base.rkt"
        ; "nav3-page1.rkt"
         "nav3-page2.rkt"
         "nav3-side.rkt"
         )
;racket/draw
(provide (rename-out [nv nv3]
                     [init! init3!]) reload-nv3)