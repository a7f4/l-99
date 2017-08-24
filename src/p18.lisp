;;; Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements
;; between the I'th and K'th element of the original list (both limits included).
;; Start counting the elements with 1.

;; Example: (slice '(a b c d e f g h i k) 3 7) -> (C D E F G)

(defun my-slice (lst left right)
  (labels ((inner (src out indx)
	     (let ((each (car src))
		   (tail (cdr src)))

	       (cond ((< indx left) (inner tail out (1+ indx)))
		     ((and (>= indx left) (<= indx right))
		      (inner tail
			     (append out (list each))
			     (1+ indx)))
		     (t out)))))

    (and (listp lst)
	 (and (numberp left) (> left 0))
	 (and (numberp right) (> right 0))
	 (>= right left)
	 (<= right (length lst)))
    (inner lst '() 1)))
