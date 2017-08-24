;;; Drop every N'th element from a list.
;; Example: (drop '(a b c d e f g h i k) 3) -> (A B D E G H K)

(defun my-drop (lst nth)
  (labels ((inner (src out indx)
	     (let ((each (car src))
		   (tail (cdr src)))

	       (if (null src)
		   out
		   (inner tail
			  (append out (if (= (mod indx nth) 0)
					  '()
					  (list each)))
			  (1+ indx))))))
    (and (listp lst)
	 (and (numberp nth) (> nth 0))
	 (inner lst '() 1))))
