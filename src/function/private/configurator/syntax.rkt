#lang racket
(require "interface.rkt"
         "mixin.rkt"
         "configurer.rkt"
         (for-syntax racket/syntax))

;配置项描述语法
;--------------------------------immutable只读配置------------------------------------
(define-syntax (define-immutable-configurer stx)
   (syntax-case stx ()
     [(_ configurer obj)
      (with-syntax  ([defconfigurer (format-id stx "define-~a" (syntax->datum #'configurer))]
                     [table (format-id stx "~a-table" (syntax->datum #'configurer))]
                     [geter (format-id stx "get-~a" (syntax->datum #'configurer))]
                     [get-immutable-sub (format-id stx "~a-immutable-sub" (syntax->datum #'configurer))]
                     )
        #'(begin 
            (define configurer obj)
            (define(table)
              (send configurer get-table))
            (define(geter field-name)
              (send configurer get-field field-name))
            (define(get-immutable-sub  key)
              (send configurer get-immutable-sub 'key))
            (define-syntax-rule (defconfigurer name default)
              (define name (let((try (geter (syntax->datum  #'name))))
                             (if try 
                                 try
                                 
                                        default)
                                 )))))
      ]))
;-----------------------------------sub子配置--------------------------------

(define-syntax (define-sub-configurer stx)
   (syntax-case stx ()
     [(_ configurer obj)
     (with-syntax  ([defconfigurer (format-id stx "define-~a" (syntax->datum #'configurer))]
                     [table (format-id stx "~a-table" (syntax->datum #'configurer))]
                     [get-sub (format-id stx "~a-sub" (syntax->datum #'configurer))]
                     [get-immutable-sub (format-id stx "~a-immutable-sub" (syntax->datum #'configurer))]
                     [get-init-sub (format-id stx "~a-init-sub" (syntax->datum #'configurer))]
                     [seter (format-id stx "set-~a!" (syntax->datum #'configurer))]
                     [initer (format-id stx "init-~a!" (syntax->datum #'configurer))]
                     [geter (format-id stx "get-~a" (syntax->datum #'configurer))]
                     [geter! (format-id stx "get-~a!" (syntax->datum #'configurer))]
                     )
        #'(begin
           (define configurer obj)
            (define(table)
              (send configurer get-table))
             (define(geter name)
              (send configurer get-field name))
            (define(geter! name init)
              (send configurer get-field! name init))

            (define(seter field-name field-value)
              (send configurer set-field! field-name field-value))
            
            (define(initer field-name field-value)
              (send configurer get-field! field-name field-value)
              (void))
            
            (define(get-init-sub  key)
              (send configurer get-sub-configurer! key (make-hasheq)))
            
            (define(get-immutable-sub key)
              (send configurer get-immutable-sub-configurer key))
            
            (define(get-sub  key)
              (send configurer get-sub-configurer key ))
            
            (define-syntax-rule (defconfigurer name default)
              (define name (geter! (syntax->datum  #'name) default)))
      ))]))


;---------------------------------------init初始配置-------------------------------------------------------------

(define-syntax (define-init-configurer stx)
   (syntax-case stx ()
     [(_ configurer obj)
      (with-syntax  ([get-init-sub (format-id stx "~a-init-sub" (syntax->datum #'configurer))]
                     [initer (format-id stx "init-~a!" (syntax->datum #'configurer))])
        #'(begin 
            (define configurer obj)

            (define(initer field-name field-value)
              (send configurer get-field! field-name field-value)
              (void))
            (define-syntax-rule (get-init-sub key)
             (send configurer get-sub-configurer! key (make-hasheq)))
              ))
      ]))


;-----------------------------------------main主配置-----------------------------------------------------------------
(define-syntax (define-main-configurer stx)
   (syntax-case stx ()
     [(_ configurer an-class identification path error-thunk)
      (with-syntax  ([defconfigurer (format-id stx "define-~a" (syntax->datum #'configurer))]
                     [save-configurer  (format-id stx "save-~a!" (syntax->datum #'configurer))]
                     [table (format-id stx "~a-table" (syntax->datum #'configurer))]
                     [get-sub (format-id stx "~a-sub" (syntax->datum #'configurer))]
                     [get-immutable-sub (format-id stx "~a-immutable-sub" (syntax->datum #'configurer))]
                     [get-init-sub (format-id stx "~a-init-sub" (syntax->datum #'configurer))]
                     [seter (format-id stx "set-~a!" (syntax->datum #'configurer))]
                     [geter (format-id stx "get-~a" (syntax->datum #'configurer))]
                     [geter! (format-id stx "get-~a!" (syntax->datum #'configurer))]
                     [initer (format-id stx "init-~a!" (syntax->datum #'configurer))]
                     )
        #'(begin 
            (define configurer (new ((configurer-defaul-mixin%  identification)an-class)
                                    [defaul-path path]
                                    [file-identification identification]
                                    [read-error-thunk error-thunk]
                                  
                                    ))
            (define(table)
              (send configurer get-table))
            
            (define(geter name)
              (send configurer get-field name))
            
            (define(geter! name init)
              (send configurer get-field! name init))

            (define(seter field-name field-value)
              (send configurer set-field! field-name field-value))
            
            (define (save-configurer)
              (send configurer save-default-file!))
            
            (define(initer field-name field-value)
              (send configurer get-field! field-name field-value)
              (void))
            
            (define(get-init-sub  key)
              (send configurer get-sub-configurer! key (make-hasheq)))
            
            (define(get-immutable-sub key)
              (send configurer get-immutable-sub-configurer key))
            
            (define(get-sub  key)
              (send configurer get-sub-configurer key ))
            
            (define-syntax-rule (defconfigurer name default)
              (define name (geter! (syntax->datum  #'name) default)))
      ))]))

(provide (all-defined-out))