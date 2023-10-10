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

;;; 12.4 d write find-node which takes a node name as input and returns the node if its appears in *node-list*

(defun find-node (n)
  (first (member n *node-list* :key #'node-name :test #'equal)))

(progn
  (setf *node-list* (list (make-node :name "Test 1")
			  (make-node :name "Test 2")))
  (assert (find-node "Test 1"))
  (assert (not (find-node "Test 3"))))

;;; 12.4 node data

(progn
  (init)
  (add-node 'start
	    "Does the engine turn over?"
	    'engine-turns-over
	    'engine-wont-turn-over)
  (add-node 'engine-turns-over
	    "Will the engine rune for any period of time?"
	    'engine-will-run-briefly
	    'engine-wont-run)
  (add-node 'engine-wont-run
	    "Is there gas in the tank?"
	    'gas-in-tank
	    "Fill the tank and try starting the engine again.")
  (add-node 'engine-wont-turn-over
	    "Do you hear any sound when you turn the key?"
	    'sound-when-turn-key
	    'no-sound-when-turn-key)
  (add-node 'no-sound-when-turn-key
	    "Is the battery voltage low?"
	    "Replace the battery"
	    'battery-voltage-ok)
  (add-node 'battery-voltage-ok
	    "Are the battery cables dirty or loose?"
	    "Clean the cables and tighten the connections."
	    'battery-cables-good))

;;; 12.4 e write process-node, it takes a node name as input
;;; if no node, print message not defined yet and return nil
;;; else ask the question associated with node, and return the yes or no action depending on user response

(defun process-node (name &aux (n (find-node name)))
  (cond
    ((null n)
     (format t "~&~A not defined yet" name))
    ((y-or-n-p (node-question n))
     (node-yes-case n))
    (t
     (node-no-case n))))

;;; 12.4 f write the function run, it maintains a local var current-node, whose initial-value is start
;;; it loops, calling process-node and storing the value back until string (print it) or nil is returned

(defun run ()
  (do
   ((current-node (process-node 'start)
		  (process-node current-node)))
   ((or (null current-node)
	(stringp current-node))
    (when (stringp current-node)
      (format t "~&~A" current-node)))))

;;; 12.4 g write an interactive function to add a new node

(defun valid-input (request validp)
  (format t "~&~A" request)
  (let ((input (read)))
    (if (funcall validp input) input
	(valid-input request validp))))

(defun user-new-node ()
  (add-node
   (valid-input "Node name?" #'symbolp)
   (valid-input "Node question?" #'stringp)
   (valid-input "Yes-case?" #'symbolp)
   (valid-input "No-case?" #'symbolp)))

;;; 12.4 h write nodes that conform to description in book

(add-node 'engine-will-run-briefly
	  "Does engine stall when cold but not when warm?"
	  'cold-idle
	  nil)
(add-node 'cold-idle
	  "Is the cold idle speed at least 700 rpm?"
	  nil
	  "Adjust the idle speed")

;;; 12.5 create a defstruct for captain with fields name, age, ship
;;; make the enterprise point to your new captain through its captain component.

;; from book
(defun print-starship (x stream depth)
  (format stream "#<STARSHIP ~A>"
	  (starship-name x)))

(defstruct (starship
	    (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(defun print-captain (x stream depth)
  (format stream "#<CAPTAIN \"~A\">"
	  (captain-name x)))

(defstruct (captain
	    (:print-function print-captain))
  (name nil)
  (age nil)
  (ship nil))

(progn
  (let* ((cap
	  (make-captain :name "James T. Kirk"
		  :age 35
		  :ship nil))
	(ship
	  (make-starship :captain cap
			 :name "Enterprise")))
    (setf (captain-ship cap) ship)
    (assert (equalp (format nil "~S" ship)
		   "#<STARSHIP Enterprise>"))
    (assert (equalp (format nil "~S" cap)
		    "#<CAPTAIN \"James T. Kirk\">"))
  ))
