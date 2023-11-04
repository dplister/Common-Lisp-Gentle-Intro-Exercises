;;; 12.4 a write a defstruct for a structure called node, with four components
;;; name, question, yes-case, no-case

;;; 12.4 b define a global variable *node-list* that wil hold all the nodes
;;; write a function init that initialises the network by setting *node-list* to nil

;;; 12.4 c write add-node it should return the name of the node it added

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

;;; 12.4 f write the function run, it maintains a local var current-node, whose initial-value is start
;;; it loops, calling process-node and storing the value back until string (print it) or nil is returned

;;; 12.4 g write an interactive function to add a new node

;;; 12.4 h write nodes that conform to description in book

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
