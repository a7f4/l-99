;; Generate a random permutation of the elements of a list.
;Example:
;(rnd-permu '(a b c d e f))
;(B A D C E F)

(defun get-uniq-rnd-nums (max n)
  "Returns a given number of unique random numbers which are less than `max`"
  (labels ((inner (num result)
             (let ((rnd (random max)))
               (cond ((= num n) result)
                     ((null (member rnd result)) (inner (1+ num) (cons rnd result)))
                     (t (inner num result))))))
    (and (<= n max)
         (inner 0 '()))))

(defun rnd-select (collection number)
  "Returns a given number of randomly selected elements from a list"
  (let ((indexes (get-uniq-rnd-nums (length collection) number)))
    (mapcar #'(lambda (index) (nth index collection)) indexes)))


(defun rnd-permu (lst)
  (rnd-select lst (length lst)))

