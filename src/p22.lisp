;;;Create a list containing all integers within a given range.
;;;   If first argument is smaller than second, produce a list in decreasing order.
;;;   Example:
;;;       (range 4 9) -> (4 5 6 7 8 9)

(defun range (from to)
  "Returns a list of numbers that contains numbers from `from` to `to` inclusively"
  (labels ((inner (next lst op terminate-p)
	     (if (funcall  terminate-p next to) 
		 (reverse lst) 
		 (inner (funcall op next) (cons next lst) op terminate-p))))

    (if (< from to)
	(inner from '() #'1+ #'>)
	(inner from '() #'1- #'<))))
