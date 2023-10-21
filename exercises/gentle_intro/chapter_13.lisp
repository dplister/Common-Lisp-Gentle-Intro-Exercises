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
