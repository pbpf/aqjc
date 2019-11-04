#lang racket
(require racket/date
         db/base)
(define (postgres-type-to-sqlite-type t)
  (case t
    [("integer")"integer"]
    [("real" "double")"real"]
    [("varchar" "char" "text" "date" "timestamp")"text"]
    [("bytea") "blob"]
    [else (error 'postgres-type-to-sql-type "type ~a can not convert to sqlite3" t)]))
(define(sql-date->text d)
  (format "~a-~a-~a" (sql-date-year d)(sql-date-month d)(sql-date-day d)))
(define(text->sql-date t)
  (match t
    [(pregexp  #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})$" (list _ a b c)) (apply sql-date(map string->number(list a b c)))]
    [else #f]))