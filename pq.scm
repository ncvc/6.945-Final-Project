;;; Priority queue code from here:
;;; http://programmingpraxis.com/2009/08/14/pairing-heaps/
;;; http://programmingpraxis.codepad.org/NHfZUl38
; pairing heaps

(define pq-empty '())
(define pq-empty? null?)

(define (pq-first pq)
  (if (null? pq)
      (error 'pq-first "can't extract minimum from null queue")
      (car pq)))

(define (pq-merge lt? p1 p2)
  (cond ((null? p1) p2)
        ((null? p2) p1)
        ((lt? (car p2) (car p1))
          (cons (car p2) (cons p1 (cdr p2))))
        (else (cons (car p1) (cons p2 (cdr p1))))))

(define (pq-insert lt? x pq)
  (pq-merge lt? (list x) pq))

(define (pq-merge-pairs lt? ps)
  (cond ((null? ps) '())
        ((null? (cdr ps)) (car ps))
        (else (pq-merge lt? (pq-merge lt? (car ps) (cadr ps))
                            (pq-merge-pairs lt? (cddr ps))))))

(define (pq-rest lt? pq)
  (if (null? pq)
      (error 'pq-rest "can't delete minimum from null queue")
      (pq-merge-pairs lt? (cdr pq))))

(define (list->pq lt? xs)
  (let loop ((xs xs) (pq pq-empty))
    (if (null? xs) pq
      (loop (cdr xs) (pq-insert lt? (car xs) pq)))))

(define (pq->list lt? pq)
  (let loop ((pq pq) (xs '()))
    (if (pq-empty? pq) (reverse xs)
      (loop (pq-rest lt? pq) (cons (pq-first pq) xs)))))

(define (pq-sort lt? xs)
  (pq->list lt? (list->pq lt? xs)))

; (display (pq-sort < '(3 5 7 0 6 5 34 3 6 9 67 5 4 4 3 1 2 3)))


;;; A few convenience methods - model the heap using a tuple for each element, '(key value), where key is numeric and value is any object
(define (heap-empty? heap)
  (pq-empty? (first heap)))

(define (make-heap-lt? lt?)
  (lambda (item1 item2)
    (lt? (first item1) (first item2))))

(define (make-heap lt?)
  ; pq backing, size, comparator
  (vector (pq-empty) 0 (make-heap-lt? lt?)))

(define (heap-pq heap)
  (vector-first heap))

(define (heap-size heap)
  (vector-second heap))

(define (heap-lt? heap)
  (vector-third heap))

(define (heap-find-min heap)
  (pq-first (heap-pq heap)))

(define (heap-pop-min! heap)
  (let ((min (heap-find-min heap)))
    (vector-set! heap 0 (pq-rest (heap-lt? heap) (heap-pq heap)))
    (vector-set! heap 1 (- (heap-size heap) 1))
    min))

(define (heap-insert! heap value key)
  (let ((item (list key value)))
    (vector-set! heap 0 (pq-insert (heap-lt? heap) item (heap-pq heap)))
    (vector-set! heap 1 (+ (heap-size heap) 1))))

; (define (heap-delete-item! heap item)
;   )

; (define (heap-decrease-key! value new-key))
