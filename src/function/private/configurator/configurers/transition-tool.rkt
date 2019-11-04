#lang racket

(define(list-with-identification? lst sign)
  (equal? (car lst)sign))

(define (multiple-hash->list hash-table)
  (for/list ([(key value)(in-hash hash-table)])
    (if(hash? value)
       (list key (multiple-hash->list value))
       (list key value))))

(define(list-for-hash? lst)
  (and (list? lst)
       (list? (car lst))))

(define (list->multiple-hash lst)
  (for/hasheq([item lst])
    (let((key (car item))
         (value (cadr item)))
      (if(list-for-hash? value)
         (values key (list->multiple-hash  value))
         (values key value)
         ))))

(define (multiple-hash->xexpr hash-table)
 (for/list([(key value)(in-hash hash-table)])
    (if(hash? value)
       `(,key () ,@(multiple-hash->xexpr value))
       (list key '() value))))

(define (xexpr->multiple-hash xexpr)
  (for/hasheq([item xexpr])
    (let((key (car item))
         (value (cddr item)))
      (if(null? value)
         (values key "")
      (if(list? (car value))
         (values key (xexpr->multiple-hash value))
         (values key (car value)))))))

;(xexpr->multiple-hash'((key () (c () "y")) (a () "a")))
;(list->multiple-hash '((b ((e 4))) (a 1)))
;(xexpr-with-empty-style->multiple-hash '((i ()(e ()6)) (c ()1)))
    
#|
(multiple-hash->xexpr #hash((a . "a")
                             (b . #hash((c . "y")))))

(list->multiple-hash '((b ((c 1))) (a 1)))
|#
;(list->multiple-hash '((b (i 1) (c 1)) (a 1)))

(provide (all-defined-out))
#|
(xexpr->multiple-hash
'((language-translate-config
   ()
   (english
    ()
    (help-label () "help?")
    (language-label () "language")
    (woring-string () "woring")
    (accecpt-and-exit-string () "accecpt-and-exit")
    (resource-port-label () "resource port: ")
    (username-label () "UserName:  ")
    (password-label () "PassWord:  ")
    (login-title () "MyEcampus connected")
    (mainframe-label () "MyEcampus by connor")
    (resource-port-choices () "school intestine abroad")
    (view-label () "view")
    (loger-label () "log")
    (time-and-cost-label () "time and cost")
    (configure-label () "config")
    (setting-page-label () "configer center")
    (login-label () "LogIn")
    (Hwadder-label () "MAC address: ")
    (autosave-label () "save parameter")
    (logout-label () "Logout")
    (apply-change-label () "apply"))
   (chinese-simplify () (view-label () "查看") (mainframe-label () "MyEcampus by connor")))
  (user-language () "english")
  (resource-port-name () "school")
  (username-value ())
  (Hwadder-init-value () "0000-0000-0000")
  (password-value ())
  (language-supported-config () (english () "english") (chinese-simplify () "简体中文"))
  (auto-save () "true"))
)
|#