;; Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).

;; list -> boolean
(defun palindrome-p (lst)
  (equal lst (reverse lst)))
