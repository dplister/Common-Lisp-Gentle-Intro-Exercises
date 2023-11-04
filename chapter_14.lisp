;;; 14.1 use ppmx to find the expression to which (pop x) expands

(setf *pretty-print* t)
(macroexpand '(pop x))

;;; 14.2 use ppmx to see to what expression the following defstruct expands

(macroexpand 
 '(defstruct starship
   (name nil)
   (condition 'green)))

;;; 14.3 write a set-nil macro that sets a variable to nil

(progn
  (setf a 1)
  (set-nil a)
  (assert (not a)))

;;; 14.4 write a macro called simple-rotatef that switches the values of the two variables

(progn
  (setf x 1)
  (setf y 2)
  (simple-rotatef x y)
  (assert (= x 2))
  (assert (= y 1)))

;;; 14.5 write a macro set-mutual that takes two variable names as input and expands into an expression that sets each variable to the name of the other

(progn
  (setf x 1)
  (setf y 2)
  (set-mutual x y)
  (assert (equal x 'y))
  (assert (equal y 'x)))

;;; 14.6 write a macro called variable-chain that accepts any number of inputs and should expand to setting param a to 'b, b to 'c, etc

(progn
  (setf x 1)
  (setf y 2)
  (setf z 3)
  (variable-chain x y z)
  (assert (equal x 'y))
  (assert (equal y 'z))
  (assert (equal z 'x)))

;;; 14.7 extend the following example (additions at bottom)

(progn
  (defnode start)
  (defnode have-5)
  (defnode have-10)
  (defnode have-15)
  (defnode have-20)
  (defnode end)
  (defarc start nickel have-5 "Clunk!")
  (defarc start dime have-10 "Clink!")
  (defarc start coin-return start "Nothing to return.")
  (defarc have-5 nickel have-10 "Clunk!")
  (defarc have-5 dime have-15 "Clink!")
  (defarc have-5 coin-return start "Returned five cents.")
  (defarc have-10 nickel have-15 "Clunk!")
  (defarc have-10 dime have-20 "Clink!")
  (defarc have-10 coin-return start "Returned ten cents.")
  (defarc have-15 nickel have-20 "Clunk!")
  (defarc have-15 dime have-20 "Nickel change.")
  (defarc have-15 gum-button end "Deliver gum.")
  (defarc have-15 coin-return start "Returned fifteen cents.")
  (defarc have-20 nickel have-20 "Nickel returned.")
  (defarc have-20 dime have-20 "Dime returned.")
  (defarc have-20 gum-button end "Deliver gum, nickel change.")
  (defarc have-20 mint-button end "Deliver mints.")
  (defarc have-20 coin-return start "Returned twenty cents."))

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>"
	  (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>"
	  (node-name (arc-from arc))
	  (arc-label arc)
	  (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialise ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(initialise)

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
	 (to (find-node to-name))
	 (new-arc (make-arc :from from
			    :label label
			    :to to
			    :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
	  (nconc (node-outputs from)
		 (list new-arc)))
    (setf (node-inputs to)
	  (nconc (node-inputs to)
		 (list new-arc)))
    new-arc))

(defun fsm (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
  (format t "~&State ~A. Input: "
	  (node-name *current-node*))
  (let* ((ans (read))
	 (arc (find ans
		    (node-outputs *current-node*)
		    :key #'arc-label)))
    (unless arc
      (format t "~&No arc from ~A has label ~A.~%"
	      (node-name *current-node*) ans)
      (return-from one-transition nil))
    (let ((new (arc-to arc)))
      (format t "~&~A" (arc-action arc))
      (setf *current-node* new))))

;;; - allow the selling of chocolate bars for 25 cents.
;;; - make it accept quarters as well as nickels and dimes
;;;    - a quarter should go "Ker-chunk!"

;; 14.11 note: this exercise does not seem to assume you have completed 14.07 (the asserts skip those nodes)
;; 14.11 a write a function compile-arc that takes an arc as inpout and returns a cond clause matching the below test

(assert (equal
	 (compile-arc (first *arcs*))
	 `((equal this-input 'nickel) (format t "~&~A" "Clunk!") (have-5 (rest input-syms)))))

;; 14.11 b write a function compile-node that takes a node as input and returns a defun expression for that node

(assert (equal (compile-node (find-node 'start))
	       `(defun start (input-syms 
			      &aux (this-input (first input-syms)))
		  (cond ((null input-syms) 'start)
			((equal this-input 'nickel)
			 (format t "~&~A" "Clunk!")
			 (have-5 (rest input-syms)))
			((equal this-input 'dime)
			 (format t "~&~A" "Clink!")
			 (have-10 (rest input-syms)))
			((equal this-input 'coin-return)
			 (format t "~&~A" "Nothing to return.")
			 (start (rest input-syms)))
			(t (error "No arc from ~A with label ~A."
				  'start this-input))))))

;; 14.11 c write a macro compile-machine that expands into a progn containing a defun for each node in *nodes*

;; 14.11 d compile and run the vending machine, what does the following expression produce?

(start '(dime dime dime gum-button))
