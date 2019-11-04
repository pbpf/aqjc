#lang racket/base

(provide (all-defined-out))

(define main-frame-label "安全监察")

(define nav-button-width 140)
(define nav-button-height 50)
(define main-frame-min-width (* nav-button-width 8))
(define side-button-width 150)
(define side-button-height 45)
(define side-panel-min-width 120)
(define side-panel-min-height 620);(* 9 side-button-height)
(define info-min-width (- main-frame-min-width side-button-width))
(define info-min-height side-panel-min-height)


(define main-frame-min-height 550);(+ (* 9 side-button-height) nav-button-height)

(define nv11-list-height 100);(floor(/ info-min-height 2))
(define nv11-view-height 450);(- info-min-height nv11-list-height)