;;; Replicate the elements of a list a given number of times.
;; Example: (repli '(a b c) 3) -> (A A A B B B C C C)

(defun my-replicate (lst num)
  (labels ((expand (num item)
	     (if (/= num 0)
		 (cons item (expand (1- num) item))
		 nil))
	   (inner (src out num)
	     (if (null src)
		 out
		 (inner (cdr src)
			(append out (expand num (car src)))
			num))))

    (and (listp lst)
	 (inner lst '() num))))
