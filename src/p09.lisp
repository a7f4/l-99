;; Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
;; Example: (pack '(a a a a b c c a a d e e e e)) -> ((A A A A) (B) (C C) (A A) (D) (E E E E))
;; list -> list

(defun my-pack (lst)
  (labels ((inner (src out buff prev)
	     
	     (let ((each (car src))
		   (tail (cdr src)))

	       (cond ((null src) (append out (list buff)))
		     ((equal each prev) (inner tail
					       out
					       (cons each buff)
					       prev))
		     (t (inner tail
			       (append out (list buff)) 
			       (list each)
			       each))))))


    (and (list lst) (not (null lst))
	 (inner lst '() '() (car lst)))))
