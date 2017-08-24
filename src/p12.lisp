;; Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
;; Example: (my-decode '((4 A) B (2 C) (2 A) D (4 E))) -> (a a a a b c c a a d e e e e)

(defun my-decode (lst)
  (labels ((my-expand (qty item)
	     (labels ((inner (out qty)
			(if (= qty 0)
			    out
			    (inner (cons item out) (1- qty)))))
	       (inner '() qty)))

	   (inner (src out)
	     (if (null src)
		 out
		 (inner (cdr src)
			(append out (if (consp (car src))
					(apply #'my-expand (car src))
					(list (car src))))))))

    (and (listp lst)
	 (inner lst '()))))

