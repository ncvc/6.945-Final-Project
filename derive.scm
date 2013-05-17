
; Summarizes the derived expression
(define (generate-summary1 expr derived-expr prev summary)
	(list (num-terms derived-expr) (length (backtrace expr prev)) (operator-complexity derived-expr)))

; Determines when to show the user a result
(define (good-enough1 expr prev summary)
	(= 1 (first (hash-table/get summary expr #f))))
