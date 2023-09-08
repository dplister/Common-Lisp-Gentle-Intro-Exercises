
(defun same-elements (l1 l2 &optional (comparer 'equal))
  "determines if two lists contain the same elements"
  (and
   (subsetp l1 l2 :test comparer)
   (subsetp l2 l1 :test comparer)
   (= (length l1) (length l2))))

(assert (same-elements '() '()))
(assert (same-elements '(1 2 3) '(3 1 2) #'equal))
(assert (same-elements '("ASDF") '("ASDF") #'string=))
(assert (not (same-elements '(1 2 3) '(1 2 2 3))))
