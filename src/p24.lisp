;;;Lotto: Draw N different random numbers from the set 1..M.
;;;    The selected numbers shall be returned in a list.
;;;    Example:
;;;        (lotto-select 6 49)
;;;        (23 1 17 33 21 37)

(defun lotto-select (n max)
  "Returns a given number of unique random numbers which are less than `max`"
  (labels ((inner (num result)
	     (let ((rnd (1+ (random max))))
	       (cond ((= num n) result)
		     ((null (member rnd result)) (inner (1+ num) (cons rnd result)))
		     (t (inner num result))))))
    (and (<= n max)
	 (inner 0 '()))))
