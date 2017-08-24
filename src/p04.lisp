;; Find the number of elements of a list.
;; Example: (my-length '(a b c)) -> 3
;; list -> number

(defun my-length (lst)
  (labels ((inner (lst qty)
	     (if (null lst)
		 qty
		 (inner (cdr lst) (1+ qty)))))
    (and (listp lst)
	 (inner lst 0))))
