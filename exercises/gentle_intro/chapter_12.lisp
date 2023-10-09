;;; 12.4 a write a defstruct for a structure called node, with four components
;;; name, question, yes-case, no-case

(defstruct node
  (name "")
  (question "")
  (yes-case nil)
  (no-case nil))

;;; 12.4 b define a global variable *node-list* that wil hold all the nodes
;;; write a function init that initialises the network by setting *node-list* to nil

(defvar *node-list* nil)

(defun init ()
  (setf *node-list* nil))

;;; 12.4 c write add-node it should return the name of the node it added

(defun add-node (name question yes-case no-case)
  (let ((n (make-node :name name
		      :question question
		      :yes-case yes-case
		      :no-case no-case)))
    (push n *node-list*)
    n))

(progn
  (init)
  (let ((exp (make-node
	      :name "test 1"
	      :question "test 2"
	      :yes-case 'a
	      :no-case 'b)))
    (assert (equalp
	     (add-node (node-name exp)
		       (node-question exp)
		       (node-yes-case exp)
		       (node-no-case exp))
	     exp))
    (assert (equalp *node-list*
		    (list exp)))))

  
