;; list -> list
;; example: (my-last '(a b c d)) -> (D)
(defun my-last (lst)
  (if (not (cdr lst))
      lst
      (my-last (cdr lst))))
