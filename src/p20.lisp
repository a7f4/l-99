;;; Remove the K'th element from a list.
;; Example: (remove-at '(a b c d) 2) -> (A C D)

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


(defun my-remove-at (lst nth)
  (and (listp lst) (and (numberp nth) (>= nth 1))
       (let ((two-parts (my-split lst (1- nth))))
	 (if (= nth 1)
	     (cdr lst)
	     (append (first two-parts) (cdr (second two-parts)))))))


