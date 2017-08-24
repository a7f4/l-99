;; Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element.
;; The order of the elements should not be changed.
;; Example: (compress '(a a a a b c c a a d e e e e)) -> (A B C A D E)

;; list -> list

(defun my-compress (lst)
  (labels ((inner (src out prev)
	     (let ((each (car src))
		   (tail (cdr src)))

	       (cond ((null src) out)
		     ((not (equal each prev)) (inner tail
						     (append out (list each))
						     each))
		     (t (inner tail out prev))))))

    (and (listp lst)
	 (inner lst '() nil))))
