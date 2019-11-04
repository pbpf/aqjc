#lang racket
(require "interface.rkt")

(define (hash->hash-mutable hash)
   (hash-copy(for/hash([(key value)(in-hash hash)])
       (if(hash? value)
          (values key (hash->hash-mutable value))
          (values key value)))))

(define (configurer-defaul-mixin% identification)
  (mixin (configurer-file<%>)(configurer-default-file<%>)
    (init-field defaul-path [read-error-thunk
                             (lambda(e)(void))]
                )
    
    [inherit  read-table load-file! save-file!]
    
    (define/public (load-default-file! read-error-thunk)
      (load-file!  defaul-path read-error-thunk))
    
    (define/public (save-default-file!)
      (save-file! defaul-path #:exists 'replace))
    
    (super-new [field-table 
                (with-handlers([exn:fail? (lambda(e)
                                           (read-error-thunk e)
                                           (make-hasheq))])
                              (hash->hash-mutable(read-table defaul-path identification)))])
               
               ))

(provide configurer-defaul-mixin%)
    
  