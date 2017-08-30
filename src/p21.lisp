;;; Insert an element at a given position into a list
;; Example:  (insert-at 'alfa '(a b c d) 2) -> (A ALFA B C D)

(defun my-split (lst len)
  (labels ((inner (src out indx)
	     
	     (let ((each (car src))
		   (tail (cdr src)))

	       (if (or (null src)
		       (> indx len))
		   (append (list out) (list src))
		   (inner tail (append  out (list each)) (1+ indx))))))

    (and (listp lst)  (and (numberp len) (> len 0)) 
	 (inner lst '() 1))))


(defun my-insert-at (elem lst nth)
  (and (listp lst) (and (numberp nth)
			(>= nth 1))
       (let ((two-parts (my-split lst (1- nth))))
	 (if (= nth 1)
	     (cons elem lst)
	     (append (first two-parts) (list elem) (second two-parts))))))
