;;; A search method that tries to minimize complexity and maximize usefulness

(load-option 'hash-table)


(define (do-search expr score-derivation score-lt? good-enough)
	(let ((to-expand (make-heap score-lt?))
	      (expanded (make-equal-hash-table))
	      (score (make-equal-hash-table))
	      (prev (make-equal-hash-table)))
		(heap-insert to-expand expr 0)
		(hash-table/put! score expr 0)
		(search to-expand expanded score prev score-derivation good-enough)))


;; Best-first search
(define (search to-expand expanded score prev score-derivation good-enough)
	(define (search-helper)
		(if (heap-empty? to-expand)
			'())
		(let ((expr (second (heap-pop-min! to-expand))))
			(hash-table/put! expanded expr #t)
			(if (good-enough expr prev score)
				(backtrace (list expr '()) prev))
			(let loop ((rules (get-applicable-rules expr)))
				(let ((derived-expr (apply-rule expr (first rules))))
					(let ((alt-score (score-derivation expr derived-expr prev score))
						  (current-score (hash-table/get score derived-expr #f)))
						(cond
							((= #f current-score)
								(hash-table/put! prev derived-expr (list expr rule))   ;; Save a tuple of the expression and rule used to get to the derived expression
								(hash-table/put! score derived-expr alt-score)
								(if (not (hash-table/get expanded derived-expr #f))
									(heap-insert to-expand derived-expr alt-score)))
							((< alt-score (hash-table/get score derived-expr #f))
								(hash-table/put! prev derived-expr (list expr rule))   ;; Save a tuple of the expression and rule used to get to the derived expression
								(hash-table/put! score derived-expr alt-score)
								(if (not (hash-table/get expanded derived-expr #f))
									(heap-decrease-key! to-expand derived-expr alt-score))))))
				(loop (cdr rules))))
		(search-helper))
	(search-helper))


;; Returns a list of tuples of the form '(expr rule)
(define (backtrace expr prev)
	(let ((prev-expr (hash-table/get prev (first expr) #f)))
		(if (not (prev-expr))
			expr
			(list (backtrace prev-expr prev) expr))))


; Compares summaries of the expressions
(define (score-list-lt? score1 score2)
	(define (score-lt-helper s1 s2)
		(if (and (= (first s1) (first s2)) (pair? (cdr s1)))
			(score-lt-helper (cdr s1) (cdr s2))
			(< (first s1) (first s2))))
	(score-lt-helper score1 score2))


; Summarizes the derived expression
(define (score-derivation1 expr derived-expr prev score)
	(list (num-terms derived-expr) (length (backtrace expr)) (operator-complexity derived-expr)))

; Determines when to show the user a result
(define (good-enough1 expr prev score)
	(= 1 (first (hash-table/get score expr #f))))

;(do-search expr score-derivation1 score-list-lt? good-enough1)


;;;Simple test cases on non-equations
(define (get-applicable-rules))