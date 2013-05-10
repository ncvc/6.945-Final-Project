;arguments are an expression or a list of expressions and the rule 
(define apply-rule (make-generic-operator 2))

(defhandler apply-rule derivative is-differentiable? derivative?)

(define (is-differentiable? expression) #t)

(define (derivative? rule) (equal? rule 'D))

(define (derivative expression rule)
  (D expression))



