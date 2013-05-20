;; Miscellaneous utilitie

(define (repeat count thunk finally)
  (do ((i 0 (+ i 1)))
      ((>= i count) (finally))
    (thunk i)))

(define (always x) (lambda () x))

(define (id x) x)

(define-syntax set-default-value!
  (syntax-rules ()
    ((_ var default)
     (if (default-object? var) (set! var default)))))

(define (random-ref lst)
  (list-ref lst
            (random (length lst))))

(define *fp-tolerance* 1e-7)

(define (fp-eq? x y)
  (<= (abs (- x y)) *fp-tolerance*))
