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

(progn
  (setf (get 'alpha 'fooprop) '(a b c d e))
  (subprop 'alpha 'd 'fooprop)
  (assert (equal (get 'alpha 'fooprop) '(a b c e))))

;;; 13.2 write a function called forget-meeting that forgets that two particularp ersons have ever met each other. Use subprop in your solution.

(progn
  (remprop 'has-met 'little-red)
  (remprop 'has-met 'wolfie)
  (record-meeting 'little-red 'wolfie)
  (assert (equal (get 'little-red 'has-met) '(wolfie)))
  (forget-meeting 'little-red 'wolfie)
  (assert (equal (get 'little-red 'has-met) nil)))

;;; 13.3 using symbol-plist, write your own version of the get function

(progn
  (setf (get 'beta 'asdf) 1)
  (assert (equal (my-get 'beta 'asdf) 1)))

;;; 13.4 write a predicate hasprop that returns t or nil to indicate whether a symbol has a particular property

(progn
  (setf (get 'ceta 'a) 1)
  (assert (hasprop 'ceta 'a))
  (assert (not (hasprop 'ceta 'b))))

;;; 13.8 a write expressions to set up a global variable *hist-array* that holds the array of counts, and a global variable *total-points* that holds the number of points recoreded so far

(defvar *hist-array* nil)
(defvar *total-points* 0)

;;; 13.8 b write a function new-histogram to initialize these variables appropriately, it should take one input: the number of bins the histogram is to have

(progn
  (new-histogram 10)
  (assert (= (length *hist-array*) 10))
  (dotimes (n 10)
    (assert (= (aref *hist-array* n) 0))))

;;; 13.8 c write the function record-value that takes a number as input and updates *hist-array* and *total-points*, if number is out of range issue an error message

(progn
  (new-histogram 10)
  (assert (= (record-value 5) 1))
  (assert (= (aref *hist-array* 5) 1))
  (assert (= *total-points* 1)))
(progn
  (new-histogram 10)
  (assert (equal (record-value 20) 'out-of-range)))

;;; 13.8 d write a function print-hist-line that takes a value from zero to ten as input, looks up the value in the array, and prints the corresponding line of the historgram

(progn
  (new-histogram 10)
  (setf (aref *hist-array* 5) 23)
  (assert (equal (print-hist-line 5 nil) " 5 [ 23] ***********************")))

;;; 13.8 e write print-histogram

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

(progn
  (create-cipher)
  (assert (typep *encipher-table* 'hash-table))
  (assert (typep *decipher-table* 'hash-table))
  (assert (= (hash-table-count *encipher-table*) 0))
  (assert (= (hash-table-count *decipher-table*) 0)))

;;; 13.9 b write a function make-substitution that takes two characters as input and stores the appropriate entries in decipher and encipher tables

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (assert (equal (gethash #\a *decipher-table*) #\b))
  (assert (equal (gethash #\b *encipher-table*) #\a)))

;;; 13.9 c write a function undo-substitution that takes one letter as input and removes the corresponding mapping from both tables

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (undo-substitution #\a)
  (assert (not (gethash #\a *decipher-table*)))
  (assert (not (gethash #\b *encipher-table*))))
  
;;; 13.9 d look up documentation from clrhash function, write clear that clears the two cipher tables

(progn
  (create-cipher)
  (make-substitution #\a #\b)
  (clear)
  (assert (= (hash-table-count *encipher-table*) 0))
  (assert (= (hash-table-count *decipher-table*) 0)))

;;; 13.9 e write a function decipher-string that takes a single encoded string as input and returns a new, partially decoded stirng

(progn
  (create-cipher)
  (make-substitution #\z #\i)
  (assert (equal (decipher-string "zj ze")
		 "i  i ")))

;;; 13.9 f write a function show-line that shows the input and the deciphered version underneath

;;; 13.9 g write a function show-text that displays all the crypto lines and their deciphered versions

;;; 13.9 h type in the following

;;; 13.9 i write a function read-letter that gets input 
;;; if symbol 'end or 'undo return the value
;;; else call get-first-character and return the result

;;; 13.9 j write a function sub-letter that takes a character as input
;;; if character already deciphered, print error and state current mapping
;;; else ask what character should map to
;;;   should be character not already mapped

;;; 13.9 k write a function undo-letter that asks "undo which letter?" and reads a letter, if character mapped, call undo-substitution, else error

;;; 13.9 l write the main function solve that takes a cryptogram as input. Solve should perform the following loop:
;;; display crypto gram, ask sub a letter? and call read-letter.
;;;   if result is a character, call sub-letter
;;;   if result is undo, call undo-letter
;;;   if result is end, return t
;;;   else, display error message

;;; 13.9 m solve the cryptogram
