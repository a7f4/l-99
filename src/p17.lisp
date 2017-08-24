;;; Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.
;; Example: (split '(a b c d e f g h i k) 3) -> ( (A B C) (D E F G H I K))

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
