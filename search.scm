;;; A search method that tries to minimize complexity and maximize usefulness

(load-option 'hash-table)

;; Helper method to pretty-print a hash table - useful for debugging
(define (hash-table-pp table)
	(display "==========\n")
	(hash-table/for-each table (lambda (key val) (display key) (display " : ") (display val) (newline)))
	(display "==========\n"))


;; Initializes and starts search
(define (search node generate-summary summary-lt? good-enough max-nonimproved-expansions get-neighbors traverse-edge)
	; Init
	(let ((to-expand (make-heap summary-lt?))
	      (expanded (make-equal-hash-table))
	      (summary (make-equal-hash-table))
	      (prev (make-equal-hash-table)))
		(hash-table/put! prev node #f)
		(let ((init-node-summary (generate-summary node node prev summary)))
			(hash-table/put! summary node init-node-summary)
			(heap-insert! to-expand init-node-summary node))

		; Begin search 
		(let loop ((nonimproved-expansions 0)
		           (best-so-far node))
			(display "best-so-far: ")
			(display best-so-far)
			(newline)
			(if (or (> nonimproved-expansions max-nonimproved-expansions) (heap-empty? to-expand))
				(begin
					(pp "done!")
					(hash-table-pp prev)
					(backtrace best-so-far prev))
				(begin 
					(let ((node (heap-pop-min! to-expand)))
						(hash-table/put! expanded node #t)
						(if (good-enough node prev summary)
							(begin
								(pp "good enough")
								(set! nonimproved-expansions max-nonimproved-expansions)
								(set! best-so-far node))
							(let inner-loop ((edges (get-neighbors node)))
								(if (not (null? edges))
									(begin
										(let* ((edge (first edges))
										       (new-node (traverse-edge node edge))
										       (alt-summary (generate-summary node new-node prev summary))
										       (current-summary (hash-table/get summary new-node #f)))
											(cond
												((not current-summary)
													(hash-table/put! prev new-node (list node edge))   ;; Save a tuple of the node and edge used to get to the new node
													(hash-table/put! summary new-node alt-summary)
													(if (not (hash-table/get expanded new-node #f))
														(heap-insert! to-expand alt-summary new-node)))
												((summary-lt? alt-summary (hash-table/get summary new-node #f))
													(hash-table/put! prev new-node (list node edge))   ;; Save a tuple of the node and edge used to get to the new node
													(hash-table/put! summary new-node alt-summary)
													(if (not (hash-table/get expanded new-node #f))
														(heap-decrease-key! to-expand new-node alt-summary))))
											(if (or (null? best-so-far) (summary-lt? alt-summary (hash-table/get summary best-so-far (list 0 0))))
												(begin
													(set! best-so-far new-node)
													(set! nonimproved-expansions 0))))
									(inner-loop (cdr edges)))))))
					(loop (+ nonimproved-expansions 1) best-so-far))))))


;; Returns a list of tuples of the form '(node edge)
(define (backtrace node prev)
	(let ((prev-node-edge (hash-table/get prev node #f)))
		(if (not prev-node-edge)
			(list (cons node '()))
			(append (backtrace (first prev-node-edge) prev) (list (cons node (second prev-node-edge)))))))



; Useful helper function that compares summaries in the form of a list of numbers, where the first elements of the lists that differ arer compared numerically
(define (summary-list-lt? summary1 summary2)
	(define (summary-lt-helper s1 s2)
		(if (and (= (first s1) (first s2)) (pair? (cdr s1)))
			(summary-lt-helper (cdr s1) (cdr s2))
			(< (first s1) (first s2))))
	(summary-lt-helper summary1 summary2))



;;; Super basic test cases for the search infrastructure (without equations) - basically just tries to search the integers for 100, starting at 10.
(define (get-applicable-edges node)
	(list -1 1))

(define (apply-edge node edge)
	(+ node edge))

; Summarizes the new node
(define (generate-summary-numeric node new-node prev summary)
	(list (abs (- new-node 100)) (length (backtrace new-node prev))))

; Determines when to show the user a result
(define (good-enough-numeric node prev summary)
	(= 0 (first (hash-table/get summary node (list 1 1)))))

; (do-search node generate-summary summary-list-lt? good-enough)
(pp (search 10 generate-summary-numeric summary-list-lt? good-enough-numeric 10 get-applicable-edges apply-edge))
