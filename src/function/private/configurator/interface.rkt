#lang racket

(define configurer-immutable<%> (interface()
                        get-table
                        get-field 
                        get-immutable-sub-configurer))

(define configurer<%> (interface(configurer-immutable<%>)
                        load-table!
                        get-field!
                        set-field! 
                        get-sub-configurer! get-sub-configurer
                        set-sub-configurer!))

(define configurer-file<%> (interface(configurer<%>)read-table load-file! save-file!))

(define configurer-default-file<%> (interface(configurer-file<%>)load-default-file! save-default-file!))

(provide (all-defined-out))