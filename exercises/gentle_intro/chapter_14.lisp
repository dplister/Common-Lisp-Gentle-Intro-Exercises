;;; 14.1 use ppmx to find the expression to which (pop x) expands

(setf *pretty-print* t)
(macroexpand '(pop x))

;; (LET ((#:G428 (CAR X)))
;;  (PROGN (SETQ X (CDR X)) #:G428))

;;; 14.2 use ppmx to see to what expression the following defstruct expands

(macroexpand 
 '(defstruct starship
   (name nil)
   (condition 'green)))

;; 
;; (PROGN
;;  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;    (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
;;     '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {700C0B2A23}>))
;;  (SB-KERNEL::%DEFSTRUCT
;;   '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {700C0B2A23}>
;;   '#(#<SB-KERNEL:LAYOUT (ID=0) for T {7003033BD3}>
;;      #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {7003033C53}>)
;;   (SB-C:SOURCE-LOCATION))
;;  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;    (SB-KERNEL::%COMPILER-DEFSTRUCT
;;     '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {700C0B2A23}>
;;     '#(#<SB-KERNEL:LAYOUT (ID=0) for T {7003033BD3}>
;;        #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {7003033C53}>)))
;;  (SB-C:XDEFUN COPY-STARSHIP
;;      :COPIER
;;      NIL
;;      (SB-KERNEL:INSTANCE)
;;    (COPY-STRUCTURE (THE STARSHIP SB-KERNEL:INSTANCE)))
;;  (SB-C:XDEFUN STARSHIP-P
;;      :PREDICATE
;;      NIL
;;      (SB-KERNEL::OBJECT)
;;    (TYPEP SB-KERNEL::OBJECT 'STARSHIP))
;;  (SB-C:XDEFUN (SETF STARSHIP-NAME)
;;      :ACCESSOR
;;      (NAME NIL)
;;      (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;    (LET ((#:INSTANCE (THE STARSHIP SB-KERNEL:INSTANCE))
;;          (#:VAL SB-KERNEL::VALUE))
;;      (SB-KERNEL:%INSTANCE-SET #:INSTANCE 1 #:VAL)
;;      #:VAL))
;;  (SB-C:XDEFUN STARSHIP-NAME
;;      :ACCESSOR
;;      (NAME NIL)
;;      (SB-KERNEL:INSTANCE)
;;    (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 1))
;; (continues on)

;;; 14.3 write a set-nil macro that sets a variable to nil

(defmacro set-nil (v)
  (list 'setq v nil))

(progn
  (setf a 1)
  (set-nil a)
  (assert (not a)))

;;; 14.4 write a macro called simple-rotatef that switches the values of the two variables

(defmacro simple-rotatef (a b)
  `(let ((temp-a ,a)
	 (temp-b ,b))
     (setq ,b temp-a)
     (setq ,a temp-b)))

(progn
  (setf x 1)
  (setf y 2)
  (simple-rotatef x y)
  (assert (= x 2))
  (assert (= y 1)))

;;; 14.5 write a macro set-mutual that takes two variable names as input and expands into an expression that sets each variable to the name of the other

(defmacro set-mutual (a b)
  `(progn
     (setq ,a ',b)
     (setq ,b ',a)))

(progn
  (setf x 1)
  (setf y 2)
  (set-mutual x y)
  (assert (equal x 'y))
  (assert (equal y 'x)))

;;; 14.6 write a macro called variable-chain that accepts any number of inputs and should expand to setting param a to 'b, b to 'c, etc

(defmacro variable-chain (&rest variables)
  `(progn
     ,@(do ((nxt (cdr variables) (cdr nxt))
	    (curr variables (cdr curr))
	    (res nil))
	   ((not curr) (reverse res))
	 (if (not nxt)
	     (push `(setf ,(first curr) ',(first variables)) res)
	     (push `(setf ,(first curr) ',(first nxt)) res)))))
	     

(progn
  (setf x 1)
  (setf y 2)
  (setf z 3)
  (variable-chain x y z)
  (assert (equal x 'y))
  (assert (equal y 'z))
  (assert (equal z 'x)))
