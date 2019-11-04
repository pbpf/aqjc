#lang racket/base
(require db/base
         racket/runtime-path
         racket/file
         racket/match
         "configurator/main.rkt")

(provide (all-defined-out))

(define-runtime-path config-path "config")
(make-directory* config-path)
;主配置
(define-main-configurer main-config xml-configurer% 
                                'DrEcampus-configure
                                    (build-path config-path "frame-config.xml")
                                   (lambda(x)(void)))
#|
(define home-path (build-path (find-system-path 'temp-dir) "aqjc"))
;(define convert-path (build-path home-path "convert"))
(define cache-path (build-path home-path "cache"))
;
;(make-temporary-file
(define(init-path*)
 ;(make-parent-directory* convert-path)
 (make-directory*  cache-path))
|#
(define(sql-date<? sd1 sd2)
  (match-define (sql-date y1 m1 d1) sd1)
  (match-define (sql-date y2 m2 d2) sd2)
  (or (< y1 y2)
      (and (= y1 y2) (or (< m1 m2)(and (= m1 m2) (< d1 d2))))))
(define format-sql-date (lambda(x)(format "~a-~a-~a" (sql-date-year x) (sql-date-month x)(sql-date-day x))))

(define(type->format type)
  (case (string-downcase type)
    [("integer" "numeric" "decimal" "tinyint" "smallint" "bigint" "float" "real" "double") number->string]
    [("character" "varchar" "longvarchar" "text") values]
    [("binary" "varbinary" "longvarbinary") bytes->string/utf-8]
    [("date" )format-sql-date]
    [else (error 'type->format "unknown type ~a" type)]))
(define(type->order type)
  (case (string-downcase type)
    [("integer" "numeric" "decimal" "tinyint" "smallint" "bigint" "float" "real" "double") <]
    [("text" "character" "varchar" "longvarchar" ) string<?]
    [("binary" "varbinary" "longvarbinary") bytes<?]
    [("date")sql-date<?]
    [else (error 'type->order "unknown type ~a" type)]))

;(init-path*)