;;; Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly.
;; I.e. don't explicitly create the sublists containing the duplicates, as in problem P09,
;; but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

;; Example: (encode-direct '(a a a a b c c a a d e e e e)) -> ((4 A) B (2 C) (2 A) D (4 E))

(defun my-encode-direct (lst)
  (labels ((inner (src out prev count)
	     (let ((each (car src))
		   (tail (cdr src))
		   (item (if (= count 1)
			     prev
			     (list count prev))))

	       (cond ((null src) (append out (list item)))
		     ((equal each prev) (inner tail out each (1+ count)) )
		     (t (inner tail
			       (append out (list item))
			       each
			       1))))))

    (and (listp lst) (not (null lst))
	 (inner lst '() (car lst) 0))))
