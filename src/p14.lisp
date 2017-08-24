;;; Duplicate the elements of a list.
;; Example: (my-duplicate '(a b c c d)) -> (A A B B C C C C D D)

(defun my-duplicate (lst)
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
	 (inner lst '() 2))))
