;; Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list
;; by replacing each list with its elements (recursively).

;; Example: (my-flatten '(a (b (c d) e))) -> (A B C D E)

;; Hint: Use the predefined functions list and append.

;; list -> list
(defun my-flatten (lst)
  (labels ((inner (src out)
	     (let ((each (car src)))
	       (cond ((and  (not (null each)) (atom each))
		      (inner (cdr src) (append  out (list each))))

		     ((consp each) (inner (cdr src)
					  (append out (inner each '()))))
		     ((null src) out)))))
    (inner lst '())))
