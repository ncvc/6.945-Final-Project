;arguments are an expression or a list of expressions and the rule 
(define apply-rule (make-generic-operator 2))

(define (add-applicable-rule expression rule)
(list (car expression) (cadr expression) (cons rule (caddr expression))))

(define (get-applicable-rules expression)
(caddr expression))




