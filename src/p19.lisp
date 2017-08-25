;;; Rotate a list N places to the left.

;; Examples: 
;; (rotate '(a b c d e f g h) 3) ->  (D E F G H A B C) 
;; (rotate '(a b c d e f g h) -2) -> (G H A B C D E F)

;; Hint: Use the predefined functions length and append,
;; as well as the result of problem P17.

;; list,number -> list
(defun my-rotate (lst n)
  (and (listp lst) (numberp n)
       (let ((nth (mod (if (> n 0)
			   n
			   (+ (length lst) n))
		       (length lst))))
	 (labels ((inner (src buff indx)
		    (let ((each (car src))
			  (tail (cdr src)))

		      (cond ((or (null src) (>= indx nth))
			     (append src  buff))
			    (t (inner tail (append buff (list each)) (1+ indx)))))))

	   (inner lst '() 0)))))
