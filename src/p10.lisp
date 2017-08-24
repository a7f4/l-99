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

;; Run-length encoding of a list.
;; Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the
;; element E.
;; Example: (encode '(a a a a b c c a a d e e e e)) -> ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

;; list -> list
(defun my-encode (lst)
  (labels ((inner (fn src out)
	     (let ((each (car src))
		   (tail (cdr src)))
	       (if (null src)
		   out
		   (inner fn tail (append out
					  (list (funcall fn each))))))))

    (and (listp lst)
	 (inner (lambda (group)
		  (list (length group) (car group)))
		(my-pack lst)
		'()))))

(defun my-encode~ (lst)
  (and (listp lst)
       (mapcar (lambda (each) (list (length each) (car each)))
	       (my-pack lst))))
