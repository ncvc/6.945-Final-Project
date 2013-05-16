(load-option 'wt-tree)
(load-option 'hash-table)

;;; Internal helper methods

(define (heap-tree heap)
  (vector-first heap))

; A reverse-lookup table
(define (heap-datum-lookup heap)
  (vector-second heap))

; Hack to make multiple values with the same key work
(define (heap-make-lt? lt?)
  (lambda (item1 item2)
    (let ((key1 (first item1))
          (key2 (first item2)))
      (cond
        ((lt? key1 key2)
          #t)
        ((lt? key2 key1)
          #f)
        ((equal? (second item1) (second item2))
          #f)
        (else
          #t)))))


;;; Public methods

(define (heap-empty? heap)
  (wt-tree/empty? (heap-tree heap)))

(define (make-heap lt?)
  (vector (make-wt-tree (make-wt-tree-type (heap-make-lt? lt?))) (make-equal-hash-table)))

(define (heap-find-min heap)
  (wt-tree/min-datum (heap-tree heap)))

(define (heap-pop-min! heap)
  (let ((min (heap-find-min heap)))
    (wt-tree/delete-min! (heap-tree heap))
    (hash-table/remove! (heap-datum-lookup heap) min)
    min))

(define (heap-insert! heap key value)
  (wt-tree/add! (heap-tree heap) (list key value) value)
  (hash-table/put! (heap-datum-lookup heap) value (list key value)))

(define (heap-decrease-key! heap value new-key)
  (let ((old-key-pair (hash-table/get (heap-datum-lookup heap) value #f)))
    (wt-tree/delete! (heap-tree heap) old-key-pair)
    (heap-insert! heap new-key value)))


; Testing
; (define a (make-heap <))
; (heap-insert! a 10 "lol10")
; (heap-insert! a 14 "lol14-2")
; (heap-insert! a 14 "lol14-1")
; (heap-insert! a 9 "lol9")
; (heap-insert! a 0 "lol0-2")
; (heap-insert! a 0 "lol0-1")

; (heap-decrease-key! a "lol14-2" 3)

; (heap-pop-min! a)
