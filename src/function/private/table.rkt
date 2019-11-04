#lang racket/base
(require racket/generator
         racket/generic
         racket/vector
         racket/class)
(provide (all-defined-out))


(define-generics merge
  (merge-row-start  merge [pos])
  (merge-row-end   merge [pos])
  (merge-col-start  merge [pos])
  (merge-col-end  merge [pos])
  #:defaults[
              (vector?(define(merge-row-start m [pos #f])
                            (vector-ref m 0))
                      (define(merge-row-end m [pos #f])
                           (vector-ref m 1))
                      (define(merge-col-start m [pos #f])
                           (vector-ref m 2))
                      (define(merge-col-end m [pos #f])
                            (vector-ref m 3)))
              (list?(define(merge-row-start m [pos #f])
                            (car m))
                    (define(merge-row-end m [pos #f])
                           (cadr m))
                    (define(merge-col-start m [pos #f])
                           (caddr  m))
                    (define(merge-col-end m [pos #f])
                            (cadddr m)))])
(struct pos(row col)#:prefab)
(struct colmerge(start end)
  #:methods gen:merge
  [(define(merge-row-start merge [pos #f])
     (pos-row pos))
   (define(merge-row-end merge [pos #f])
    (pos-row pos))
   (define(merge-col-start merge [pos #f])
     (colmerge-start merge))
   (define(merge-col-end merge [pos #f])
     (colmerge-end merge))])

;table-info (listof (listof pair?))

;(struct rectangle (row-start row-end col-start col-end))



(define(table-info->map info)
  (for/hash ([(k v)
              (in-generator
               (for ([row(in-list info)]
                     [i(in-naturals 1)])
                 (for([col (in-list row)]
                      [j (in-naturals 1)])
                   (yield (cons i j)
                          col))))])
    (values k v)))


(define(cell-insert-row-item r c p)
  (if(>= p r)
     (pos r c)
     (pos (add1 r) c)))
(define(cell-del-row-item r c p)
  (if(> p r)
     (pos r c)
     (if(= p r)
        (error 'cell-del-row-item "row ~a = delete row ~a" r p)
        (pos (sub1 r) c))));-------------

(define(cell-insert-row c p)
  (cell-insert-row-item (pos-row c) (pos-col c) p))
(define(cell-del-row c p)
  (cell-del-row-item (pos-row c) (pos-col c) p))

(define(merge-column-x-size column-width-sum merge)
  (values (vector-ref column-width-sum (sub1(merge-col-start merge)))
          (- (vector-ref column-width-sum (merge-col-end merge))
          (vector-ref column-width-sum (sub1(merge-col-start merge))))))
(define(merge-column-x column-width-sum merge)
  (vector-ref column-width-sum (sub1(merge-col-start merge))))
(define(merge-column-size column-width-sum merge)
  (- (vector-ref column-width-sum (merge-col-end merge))
          (vector-ref column-width-sum (sub1(merge-col-start merge)))))
(define(merge-row-y row-height-sum merge pos)
  (vector-ref row-height-sum (sub1(merge-row-start merge pos))))
(define(merge-row-size row-height-sum merge pos)
 ; (displayln row-height-sum)
  (- (vector-ref row-height-sum (merge-row-end merge pos))
          (vector-ref row-height-sum (sub1(merge-row-start merge pos)))))

(define(scale-vector lst k)
      (vector-map (lambda(x)(floor(* x k))) lst))
(define(sum-vector lst)
      (for/sum([i (in-vector lst)])
        i))
(define(rec-get-rest-size i rec-index p sizes)
  (- p (for/sum([j (in-vector sizes (sub1(vector-ref i (sub1 rec-index))) (sub1(vector-ref i rec-index)))])
                   j)))

(define(vector-fold-sum vec len)
  (define vec2 (make-vector (add1 len) 0))
    (define sum1(for/fold([sum 0])
            ([p (in-vector vec 0 len)]
             [k (in-naturals 0)])
           (vector-set! vec2 k sum)
           (+ sum p)))
  (vector-set! vec2 len sum1)
  vec2)

;;---------------horizontal------------------
(define(computer-column-widths info children column-widths index)
      (define size-sum (vector-fold-sum column-widths index))
      (for/fold([max-t (vector-ref column-widths index)])
               ([i (in-list children)]
                [info (in-list info)]
               #:when (= (sub1(merge-col-end (send i get-merge))) index))
        (max max-t (- (car info) (- (vector-ref size-sum (sub1(merge-col-end (send i get-merge))))
                                      (vector-ref size-sum (sub1(merge-col-start (send i get-merge)))))))))

;-----------------------------------------------------------------------------------------------------
(define(computer-row-heights info children row-heights index)
      (define size-sum (vector-fold-sum row-heights index))
;  (printf "~a ~a\n" row-heights index)
      (for/fold([max-t (vector-ref row-heights index)])
               ([i (in-list children)]
                [info (in-list info)]
               #:when (= (sub1(merge-row-end (send i get-merge) (send i get-pos))) index))
        (max max-t (- (cadr info) (- (vector-ref size-sum (sub1(merge-row-end (send i get-merge)(send i get-pos))))
                                        (vector-ref size-sum (sub1(merge-row-start (send i get-merge)(send i get-pos)))))))))
;-------------------------------------------------------------------------------------------------

(define(insert-vector-after vecsrc len index)
  (define vec (make-vector (add1 len) 0))
      ;(displayln row-heights)
   (vector-copy! vec 0 vecsrc 0 index)
   (vector-copy! vec index vecsrc (sub1 index) len)
  vec)
(define(delete-vector vecsrc len index)
  (define vec (make-vector (sub1 len) 0))
   (vector-copy! vec 0 vecsrc 0 (sub1 index))
   (vector-copy! vec (sub1 index) vecsrc index len)
  vec)

