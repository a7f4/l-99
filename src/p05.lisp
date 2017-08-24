;; Reverse a list.
;; Example: (my-reverse '(a b c d)) -> (d c b a)
;; list -> list

(defun my-reverse (lst)
  (labels ((inner (src out)
	     (if (null src)
		 out
		 (inner (cdr src)
			(cons (car src)
			      out)))))
    (and (listp lst)
	 (inner lst '()))))
