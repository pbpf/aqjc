#lang racket
(require "../../interface.rkt")
         
;------------------------------------
(define (hash->hash-mutable hash)
   (hash-copy(for/hash([(key value)(in-hash hash)])
       (if(hash? value)
          (values key (hash->hash-mutable value))
          (values key value)))))
;------------------immutable-----------------------
(define immutable-configurer%
  (class* object% (configurer-immutable<%> )
    (init-field field-table)
    
    (define/public(get-table)
      field-table)
    
    (define/public(get-field field-name)
      (hash-ref field-table field-name (lambda()#f)))
    
    (define/public(get-immutable-sub-configurer key)
      (new immutable-configurer% [field-table (hash-ref field-table key (lambda()(error 'get-immutable-sub-configurer "can not find table ~a from ~a!" key field-table)))]))
      
    (super-new)))
;-------------------------------------------------------------------
;核心类型(内存模型)hash-table
;3.0 允许 field-table 有多级结构
(define hash-table-configurer%
  (class* immutable-configurer% (configurer<%>)
    
    (inherit-field field-table)
    
    (define/public(load-table! table)
      (set! field-table (hash->hash-mutable table)))
    
   
    (define/public(get-field! field-name to-set)
      (hash-ref! field-table field-name to-set))
    
    (define/public(set-field! field-name field-value)
      (hash-set! field-table field-name field-value))
    
    (define/public(get-sub-configurer! key to-set)
      (new hash-table-configurer% [field-table (hash-ref! field-table key to-set)]))
    
    (define/public(set-sub-configurer! key configer)
      (hash-set! field-table key (send configer get-table)))
    
    (define/public(get-sub-configurer key)
      (new hash-table-configurer% [field-table (hash-ref field-table key (lambda()(error 'get-sub-configurer "can not find table!")))]))
      
    (super-new )))

(provide (all-defined-out))