;;; 10.2 rewrite the lemonade stand sell function to use incf instead of setf

(setf *total-glasses* 0)

;; example from book
(defun sell (n)
  (setf *total-glasses* (+ *total-glasses* n))
  (format t "~&Total glasses sold: ~S" *total-glasses*))

(defun sell (n)
  (incf *total-glasses* n)
  (format t "~&Total glasses sold: ~S" *total-glasses*))

(progn
  (setf *total-glasses* 0)
  (sell 5)
  (assert (= *total-glasses* 5)))

;;; 10.3 modify the meet function to keep a count of howm any people have been met more than once

;; example from book
(setf *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*))
	 'we-just-met)
	((member person *friends*)
	 'we-know-each-other)
	(t
	 (push person *friends*)
	 'pleased-to-meet-you)))

(defun meet (person)
  (cond ((equal person (first (first *friends*)))
	 (incf (second (assoc person *friends*)))
	 'we-just-met)
	((assoc person *friends*)
	 (incf (second (assoc person *friends*)))
	 'we-know-each-other)
	(t
	 (push (list person 0) *friends*)
	 'pleased-to-meet-you)))

(progn
  (setf *friends* nil)
  (meet 'fred)
  (assert (equal *friends* '((fred 0))))
  (meet 'fred)
  (assert (equal *friends* '((fred 1))))
  (meet 'fred)
  (assert (equal *friends* '((fred 2))))
  (meet 'mary)
  (assert (equal *friends* '((mary 0) (fred 2)))))

;;; 10.4 write a function forget that removes a person from the *friends* list
;;; if the person wasn't on the list, the function should complain

(defun forget (person)
  (if (assoc person *friends*)
      (setf *friends*
	    (remove-if (lambda (p) (equal (first p) person))
		       *friends*))
      (format t "~%No person found for ~A" person)))

(progn
  (setf *friends* nil)
  (meet 'fred)
  (assert (equal *friends* '((fred 0))))
  (forget 'fred)
  (assert (equal *friends* '())))

;;; 10.5 rewrite the following function to use good lisp style

(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is pct 'percent 'of 'max y))

(assert (equal (ugly 20 2) '(average 11.0 is 55.0 percent of max 20)))

(defun not-ugly (x y)
  (let* ((nx (max x y))
	 (ny (min x y))
	 (avg (/ (+ ny nx) 2.0))
	 (pct (* 100 (/ avg nx))))
  (list 'average avg 'is pct 'percent 'of 'max nx)))

(assert (equal (not-ugly 20 2) '(average 11.0 is 55.0 percent of max 20)))
