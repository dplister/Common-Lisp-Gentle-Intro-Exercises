;;; 3.5 Using defun, write a definition named half that returns a number that is one-half of its input n. Use defun.

(assert (= (half 6) 3))
(assert (= (half 5) 5/2))
(assert (= (half -2) -1))

;;; 3.5 Using defun, write a definition named cube that returns a number that is to the power of three of its input n. Use defun.

(assert (= (cube 1) 1))
(assert (= (cube 3) 27))
(assert (= (cube 11) 1331))

;;; 3.5 Using defun, write a predicate named onemorep that tests if the first input is exactly one greater than its second input.

(assert (onemorep 2 1))
(assert (not (onemorep 3 1)))
(assert (onemorep -4 -5))
