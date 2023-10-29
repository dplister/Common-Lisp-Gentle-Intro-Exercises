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
