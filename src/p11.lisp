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

;; Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates it is
;; simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
;; Example: (encode-modified '(a a a a b c c a a d e e e e)) -> ((4 A) B (2 C) (2 A) D (4 E))

;; list -> list
(defun my-encode-modified (lst)
  (labels ((inner (fn src out)
	     (let ((each (car src))
		   (tail (cdr src)))
	       (if (null src)
		   out
		   (inner fn tail (append out
					  (list (funcall fn each))))))))

    (and (listp lst)
	 (inner (lambda (group)
		  (if (null (cdr group))
		      (car group)
		      (list (length group) (car group))))
		(my-pack lst)
		'()))))

(defun my-encode-modified~ (lst)
  (and (listp lst)
       (mapcar (lambda (each)
		 (if (null (cdr each))
		     (car each)
		     (list (length each) (car each))
		     ) )
	       (my-pack lst))))
