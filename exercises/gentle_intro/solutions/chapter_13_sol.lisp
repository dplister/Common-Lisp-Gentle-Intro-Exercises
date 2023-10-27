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

;;; 13.9 preamble

(defvar *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfwkj"))

;;; 13.9 a define encipher-table decipher-table variables

(defvar *encipher-table* nil)
(defvar *decipher-table* nil)

(defun create-cipher ()
  (setf *encipher-table* (make-hash-table))
  (setf *decipher-table* (make-hash-table)))

(progn
  (create-cipher)
  (assert (typep *encipher-table* 'hash-table))
  (assert (typep *decipher-table* 'hash-table))
  (assert (= (hash-table-count *encipher-table*) 0))
  (assert (= (hash-table-count *decipher-table*) 0)))

;;; 13.9 b write a function make-substitution that takes two characters as input and stores the appropriate entries in decipher and encipher tables

(defun make-substitution (from to)
  (setf (gethash from *decipher-table*) to)
  (setf (gethash to *encipher-table*) from))

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (assert (equal (gethash #\a *decipher-table*) #\b))
  (assert (equal (gethash #\b *encipher-table*) #\a)))

;;; 13.9 c write a function undo-substitution that takes one letter as input and removes the corresponding mapping from both tables

(defun undo-substitution (c)
  (let ((to (gethash c *decipher-table*)))
    (setf (gethash c *decipher-table*) nil)
    (setf (gethash to *encipher-table*) nil)))

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (undo-substitution #\a)
  (assert (not (gethash #\a *decipher-table*)))
  (assert (not (gethash #\b *encipher-table*))))
  
;;; 13.9 d look up documentation from clrhash function, write clear that clears the two cipher tables

(defun clear ()
  (clrhash *decipher-table*)
  (clrhash *encipher-table*))

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (clear)
  (assert (= (hash-table-count *encipher-table*) 0))
  (assert (= (hash-table-count *decipher-table*) 0)))

;;; 13.9 e write a function decipher-string that takes a single encoded string as input and returns a new, partially decoded stirng

(defun decipher-string (str)
  (map 'string
       (lambda (c) (or (gethash c *decipher-table*) #\ ))
       str))

(progn
  (create-cipher)
  (make-substitution #\z #\i)
  (assert (equal (decipher-string "zj ze")
		 "i  i ")))

;;; 13.9 f write a function show-line that shows the input and the deciphered version underneath

(defun show-line (str)
  (format t "~&~A" str)
  (format t "~&~A" (decipher-string str)))

;;; 13.9 g write a function show-text that displays all the crypto lines and their deciphered versions

(defun show-text (crypto)
  (dolist (l crypto)
    (show-line l)))

;;; 13.9 h type in the following

(defun get-first-char (str)
  (char-downcase (char (format nil "~A" str) 0)))

;;; 13.9 i write a function read-letter that gets input 
;;; if symbol 'end or 'undo return the value
;;; else call get-first-character and return the result

(defun read-letter ()
  (let ((input (read)))
    (cond
      ((or (equal input 'end)
	   (equal input 'undo))
       input)
      (t (get-first-char input)))))

;;; 13.9 j write a function sub-letter that takes a character as input
;;; if character already deciphered, print error and state current mapping
;;; else ask what character should map to
;;;   should be character not already mapped

(defun capture-sub ()
  (format t "~&what should letter map to?")
  (let ((c (read-letter)))
    (cond
      ((not (gethash c *encipher-table*)) c)
      (t
       (format t "~&letter already mapped: ~a" c)
       (capture-sub)))))

(defun sub-letter (c)
  (let ((d (gethash c *decipher-table*)))
    (cond
      ((not d)
       (make-substitution c (capture-sub)))
      (t
       (format t "~&letter already substituted to ~a" d)))))
  
;;; 13.9 k write a function undo-letter that asks "undo which letter?" and reads a letter, if character mapped, call undo-substitution, else error

(defun undo-letter ()
  (format t "~&Undo which letter?")
  (let* ((c (read-letter))
	 (d (gethash c *decipher-table*)))
    (cond
      ((not d)
       (format t "~&No mapping for letter ~a" c))
      (t
       (undo-substitution c)))))

;;; 13.9 l write the main function solve that takes a cryptogram as input. Solve should perform the following loop:
;;; display crypto gram, ask sub a letter? and call read-letter.
;;;   if result is a character, call sub-letter
;;;   if result is undo, call undo-letter
;;;   if result is end, return t
;;;   else, display error message

(defun solve (crypto)
  (show-text crypto)
  (format t "~&Substitute a letter?")
  (let ((response (read-letter)))
    (cond
      ((equal response 'end)
       t)
      ((equal response 'undo)
       (undo-letter)
       (solve crypto))
      ((characterp response)
       (sub-letter response)
       (solve crypto))
      (t
       (format t "Unknown input")
       (solve crypto)))))
    
;;; 13.9 m solve the cryptogram

;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to
;; enlpo pib slafml pvv bfwkj
;; speak and remove all doubt
