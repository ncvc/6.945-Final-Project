;; law structure
(define-structure law name example-sets input-specs output-specs)

;; example-set structure
(define-structure example-set name initial-conditions results-table)

;; input-spec structure
(define (input-name input)
  (car input))

(define (input-get-by-name inputs name)
  (assoc name inputs))

(define (input-generator-specs input)
  (cdr input))

(define (generator-spec-reqs generator-spec)
  (car generator-spec))

(define (generator-spec-exp generator-spec)
  (cadr generator-spec))

(define (first-input-generator input defined-vars)
  (let lp ((specs (input-generator-specs input)))
    (cond
     ((null? specs) #f)
     ((every (lambda (x) (memq x defined-vars)) (generator-spec-reqs (car specs)))
      (generator-spec-exp (car specs)))
     (else (lp (cdr specs))))))

;; initial-conditions
(define *required-vars-symbol* 'required-vars)

(define (get-required-vars initial-conditions)
  (or (assoc *required-vars-symbol* initial-conditions) '()))

(define (specified-initial-conditions initial-conditions)
  (remove (lambda (x) (eq? (car x) *required-vars-symbol*)) initial-conditions))

;; grunt functions

(define (generate-example-set name law initial-conditions count)
  (let ((op-generate (make-example-generator law initial-conditions))
        (results-table (make-vector count)))
    (repeat
     count
     (lambda (i) (vector-set! results-table i (op-generate)))
     (always #t))
    (make-example-set name initial-conditions results-table)))

(define (make-example-generator law initial-conditions #!optional verbose)
  (let* ((required-vars-exp
          (fill-vars-from-specs law (get-required-vars initial-conditions) '()))
         (specified-vars-exp
          (specified-initial-conditions initial-conditions))
         (specified-exp (append required-vars-exp specified-vars-exp))
         (unspecified-exp (fill-vars-from-specs law
                                                (get-unspecified-vars law specified-exp)
                                                (map car specified-exp)))
         (full-vars-exp (append specified-exp unspecified-exp)))
    (let ((generator-expression (construct-generator-expression full-vars-exp (law-output-specs law))))
      (if (eq? verbose #t) (pp generator-expression))
      (eval generator-expression (nearest-repl/environment)))))

(define (construct-generator-expression input-expression output-expression)
  `(lambda ()
     (let* ,input-expression
       (list ,@(map cadr output-expression)))))

(define (get-unspecified-vars law specified-exp)
  (let ((input-specs (law-input-specs law))
        (specified-vars (map car specified-exp)))
    (remove (lambda (x) (memq x specified-vars))
            (map input-name input-specs))))

(define (fill-vars-from-specs law vars defined-vars)
  (if (null? vars)
      vars
      (let ((input-specs (law-input-specs law)))
        (let lp ((defined-vars defined-vars)
                 (underspecified '())
                 (remaining-vars vars)
                 (vars-exp '()))
          (if (null? remaining-vars)
              (append vars-exp (fill-vars-from-specs law underspecified defined-vars))
              (let* ((var
                      (car remaining-vars))
                     (generator
                      (first-input-generator
                       (input-get-by-name input-specs var)
                       defined-vars)))
                (if generator
                    (lp (cons var defined-vars)
                        underspecified
                        (cdr remaining-vars)
                        (append vars-exp `((,var ,generator))))
                    (lp defined-vars
                        (cons var underspecified)
                        (cdr remaining-vars)
                        vars-exp))))))))

;; Combining code

(define (make-selection-stream n)
  (define (helper n)
    (if (= n 1)
        (list->stream '((0) (1)))
        (let ((n-1 (helper (- n 1))))
          (stream-append (stream-map (lambda (x) (cons 0 x)) n-1)
                         (stream-map (lambda (x) (cons 1 x)) n-1)))))
  (stream-cdr (helper n)))

(define (selection-stream-length n)
  (define (helper n)
    (if (> n 1)
        (* 2 (helper (- n 1)))
        2))
  (- (helper n) 1))

(define (select-items selection items)
  (filter identity
          (map (lambda (x y) (if (= y 1) x #f))
               items selection)))

(define (make-combinator-stream n)
  (stream-map
   (lambda (selection)
     (let ((nselected (count (lambda (x) (= x 1)) selection)))
      (stream-map
       (lambda (function)
         (lambda args
           (let ((selected-args (select-items selection args)))
             (apply function selected-args))))
       (make-n-function-stream nselected))))
   (make-selection-stream n)))

(define (make-exponential-stream) (stream-generate (lambda (n) (lambda (x) (expt x n)))))
(define *combinator-functions* (list + - / *))

(define (single-arity-function-stream #!optional stream-combinator)
  (set-default-value! stream-combinator stream-interleave)
  (reduce-left stream-combinator '()
               (list
                (list->stream *combinator-functions*)
                (make-exponential-stream))))

;; Hack, not a good way of doing this
(define (combinator-functions-stream arity #!optional combinator-functions)
  (set-default-value! combinator-functions *combinator-functions*)
  (list->stream combinator-functions))

(define (make-n-function-stream arity)
  (if (> arity 1)
      (combinator-functions-stream arity)
      (single-arity-function-stream)))

#|
;; Example application
;; -------------------

;; Load scmutils
(cd "/usr/local/scmutils/src/")
(load "load")

(define (X->constant-m-P m X)
  (* m (D X)))

;; Example solutions to 1-D force law
;; -----------------

;; Harmonic oscillator
(define ((harmonic-V k) x)
  (* 1/2 k (expt x 2)))

(define ((harmonic-x k m) t)
  (cos (* (sqrt (/ k m)) t)))

(define ((harmonic-p k m) t)
  ((X->constant-m-P m (harmonic-x k m)) t))

;; Constant force
(define ((constant-V a) x)
  (* a x))

(define ((constant-x a m) t)
  (* 1/2 (- (/ a m)) (expt t 2)))

(define ((constant-p a m) t)
  ((X->constant-m-P m (constant-x a m)) t))

(define harmonic-initial-conditions
  '((k 3)
    (m 3)
    (V (harmonic-V k))
    (X (harmonic-x k m))))

(define newton-F
  (make-law 'newton-F '()
            '((V)
              (P ((m X) (X->constant-m-P m X)))
              (X)
              (t (() (random-float))))
            '((Vx (V (X t)))
              (Pt (P t))
              (Xt (X t))
              (T t))))

(define harmonic-example-set (generate-example-set 'harmonic newton-F harmonic-initial-conditions 30))
|#
