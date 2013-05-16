;;; A search method that tries to minimize complexity and maximize usefulness

(load-option 'hash-table)


(define (do-search expr score-derivation score-lt? good-enough)
	(let ((to-expand (make-heap score-lt?))
	      (expanded (make-equal-hash-table))
	      (score (make-equal-hash-table))
	      (prev (make-equal-hash-table)))
		(heap-insert! to-expand 0 expr)
		(hash-table/put! score expr 0)
		(search to-expand expanded score prev score-derivation good-enough)))


;; Best-first search
(define (search to-expand expanded score prev score-derivation good-enough)
	(let loop ()
		(pp 1)
		(if (heap-empty? to-expand)
			(begin (pp prev) '())
			(begin 
				(let ((expr (heap-pop-min! to-expand)))
				      (hash-table/put! expanded expr #t)
					(if (good-enough expr prev score)
						(begin
							(backtrace (list expr '()) prev)
							(if (not (null? rules))
								(begin
									(let* ((rule (first rules))
									       (alt-score (score-derivation expr derived-expr prev score))
									       (current-score (hash-table/get score derived-expr #f)))
										(cond
											((not current-score)
												(hash-table/put! prev derived-expr (list expr rule))   ;; Save a tuple of the expression and rule used to get to the derived expression
												(hash-table/put! score derived-expr alt-score)
												(if (not (hash-table/get expanded derived-expr #f))
													(heap-insert! to-expand alt-score derived-expr)))
											((< alt-score (hash-table/get score derived-expr #f))
												(hash-table/put! prev derived-expr (list expr rule))   ;; Save a tuple of the expression and rule used to get to the derived expression
												(hash-table/put! score derived-expr alt-score)
												(if (not (hash-table/get expanded derived-expr #f))
													(heap-decrease-key! to-expand derived-expr alt-score)))))
								(inner-loop (cdr rules)))))))
				(loop)))))


;; Returns a list of tuples of the form '(expr rule)
(define (backtrace expr prev)
	(let ((prev-expr (hash-table/get prev expr #f)))
		(if (not prev-expr)
			(list expr)
			(append (backtrace prev-expr prev) expr))))


; Compares summaries of the expressions
(define (score-list-lt? score1 score2)
	(define (score-lt-helper s1 s2)
		(if (and (= (first s1) (first s2)) (pair? (cdr s1)))
			(score-lt-helper (cdr s1) (cdr s2))
			(< (first s1) (first s2))))
	(score-lt-helper score1 score2))


; Summarizes the derived expression
(define (score-derivation1 expr derived-expr prev score)
	(list (num-terms derived-expr) (length (backtrace expr prev)) (operator-complexity derived-expr)))

; Determines when to show the user a result
(define (good-enough1 expr prev score)
	(= 1 (first (hash-table/get score expr #f))))



;;;Simple test cases the search infrastructure (without equations)
(define (get-applicable-rules expr)
	(list -1 1))

(define (apply-rule expr rule)
	(+ expr rule))

; Summarizes the derived expression
(define (score-derivation-numeric expr derived-expr prev score)
	(list (abs (- expr 4)) (length (backtrace expr prev))))

; Determines when to show the user a result
(define (good-enough-numeric expr prev score)
	(= 1 expr))

; (do-search expr score-derivation score-list-lt? good-enough)
(pp (do-search 10 score-derivation-numeric score-list-lt? good-enough-numeric))
