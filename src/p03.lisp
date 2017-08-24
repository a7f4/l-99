;; Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:  (element-at '(a b c d e) 3) -> C
;; list -> item 

(defun element-at (lst position)
  (and (numberp position)
       (> position 0)
       (>= (length lst) position)

       (if (= position 1)
	   (car lst)
	   (element-at (cdr lst) (1- position)))))
