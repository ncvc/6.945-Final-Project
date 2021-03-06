;; Stream utils for mit-scheme streams

(define (stream-always x)
  (cons-stream x (stream-always x)))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-collapse stream #!optional stream-combinator)
  (set-default-value! stream-combinator stream-append)
  (if (stream-null? stream)
      stream
      (stream-combinator (stream-car stream) (stream-collapse (stream-cdr stream)))))

(define (stream-interleave s1 s2)
  (if (stream-null? s1) s2
      (cons-stream (stream-car s1)
                   (stream-interleave s2 (stream-cdr s1)))))

(define (stream-singleton x)
  (cons-stream x '()))

(define (stream-generate generator #!optional start-value increment-function)
  (set-default-value! start-value 0)
  (set-default-value! increment-function (lambda (x) (+ x 1)))
  (cons-stream (generator start-value)
               (stream-generate generator (increment-function start-value)
                                increment-function)))

(define (make-stream-interface stream #!optional length)
  (set-default-value! length #f)
  (let ((input-stream stream)
        (index 0)
        (stream-store '()))
    (let ((remaining-stream input-stream))
      (define (reset)
        (set! index 0)
        (set! remaining-stream input-stream))
      (define (store-stream key)
        (set! stream-store (cons (list key index) stream-store)))
      (define (restore-stream key)
        (let ((i (cadr (assoc key stream-store))))
          (set! index i)
          (set! remaining-stream (stream-tail input-stream index))))
      (define (stream-interface message)
        (case message
          ((next pop)
           (if (stream-null? remaining-stream) (reset))
           (let ((next-val (stream-car remaining-stream)))
             (set! remaining-stream (stream-cdr remaining-stream))
             (set! index (+ 1 index))
             next-val))
          ('reset (reset))
          ((list all)
           (stream->list input-stream))
          ('random
           (stream-ref input-stream (random (or length (+ index 300)))))
          ((store save)
           (lambda (key)
             (store-stream key)))
          ('restore
           (lambda (key)
             (restore-stream key)))
          (else
           (cond
            ((number? message)
             (stream-ref input-stream message))
            (else (error "Invalid message for stream-interface: " message))))))
      stream-interface)))
