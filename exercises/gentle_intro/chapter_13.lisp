;;; 13.1 write a function subprop that deletes an element from a set stored under a property name

;; prelim example from book

(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

(progn
  (remprop 'has-met 'little-red)
  (remprop 'has-met 'wolfie)
  (remprop 'has-met 'grandma)
  (record-meeting 'little-red 'wolfie)
  (record-meeting 'wolfie 'grandma)
  (assert (equal (symbol-plist 'wolfie) '(has-met (grandma little-red)))))

;; answer

(defun subprop (sym elem prop)
  (setf (get sym prop)
	(remove elem (get sym prop))))

(progn
  (setf (get 'alpha 'fooprop) '(a b c d e))
  (subprop 'alpha 'd 'fooprop)
  (assert (equal (get 'alpha 'fooprop) '(a b c e))))

;;; 13.2 write a function called forget-meeting that forgets that two particularp ersons have ever met each other. Use subprop in your solution.

(defun forget-meeting (x y)
  (subprop x y 'has-met)
  (subprop y x 'has-met)
  t)

(progn
  (remprop 'has-met 'little-red)
  (remprop 'has-met 'wolfie)
  (record-meeting 'little-red 'wolfie)
  (assert (equal (get 'little-red 'has-met) '(wolfie)))
  (forget-meeting 'little-red 'wolfie)
  (assert (equal (get 'little-red 'has-met) nil)))

;;; 13.3 using symbol-plist, write your own version of the get function

(defun my-get (sym prop)
  (second (member prop (symbol-plist sym))))

(progn
  (setf (get 'beta 'asdf) 1)
  (assert (equal (my-get 'beta 'asdf) 1)))

;;; 13.4 write a predicate hasprop that returns t or nil to indicate whether a symbol has a particular property

(defun hasprop (sym prop)
  (member prop (symbol-plist sym)))

(progn
  (setf (get 'ceta 'a) 1)
  (assert (hasprop 'ceta 'a))
  (assert (not (hasprop 'ceta 'b))))

;;; 13.8 a write expressions to set up a global variable *hist-array* that holds the array of counts, and a global variable *total-points* that holds the number of points recoreded so far

(defvar *hist-array* nil)
(defvar *total-points* 0)

;;; 13.8 b write a function new-histogram to initialize these variables appropriately, it should take one input: the number of bins the histogram is to have

(defun new-histogram (bins)
  (setf *hist-array* (make-array bins :initial-element 0))
  (setf *total-points* 0))

(progn
  (new-histogram 10)
  (assert (= (length *hist-array*) 10))
  (dotimes (n 10)
    (assert (= (aref *hist-array* n) 0))))

;;; 13.8 c write the function record-value that takes a number as input and updates *hist-array* and *total-points*, if number is out of range issue an error message

(defun record-value (n)
  (when (>= n (length *hist-array*))
    (return-from record-value 'out-of-range))
  (incf (aref *hist-array* n))
  (incf *total-points*)
  (aref *hist-array* n))

(progn
  (new-histogram 10)
  (assert (= (record-value 5) 1))
  (assert (= (aref *hist-array* 5) 1))
  (assert (= *total-points* 1)))
(progn
  (new-histogram 10)
  (assert (equal (record-value 20) 'out-of-range)))

;;; 13.8 d write a function print-hist-line that takes a value from zero to ten as input, looks up the value in the array, and prints the corresponding line of the historgram

(defun print-hist-line (n &optional (output t))
  (let ((cnt (aref *hist-array* n)))
    (format output "~&~2@S [~3@S] ~A" n cnt (make-string cnt :initial-element #\*))))

(progn
  (new-histogram 10)
  (setf (aref *hist-array* 5) 23)
  (assert (equal (print-hist-line 5 nil) " 5 [ 23] ***********************")))

;;; 13.8 e write print-histogram

(defun print-histogram ()
  (dotimes (n (length *hist-array*))
    (print-hist-line n))
  (format t "~&    ~3@S total" *total-points*))

(progn
  (new-histogram 10)
  (dotimes (v 200)
    (record-value (random 10)))
  (print-histogram))
