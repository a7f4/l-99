;; Find the last but one box of a list.
;; Example: (my-but-last '(a b c d)) -> (C D)
;; list -> list
(defun my-but-last (lst)
  (and (consp lst)
       (if (not (cddr lst))
	   lst
	   (my-but-last (cdr lst)))))

