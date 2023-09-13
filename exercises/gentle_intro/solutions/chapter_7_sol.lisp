;;; 7.1 write an add1 function that adds one to its input, then write a function to add one to each element of the list.

(defun add1 (x)
  (+ x 1))

(defun add1s (ls)
  (mapcar #'add1 ls))

(assert (equal (add1s '(13 5 7 9)) '(14 6 8 10)))
(assert (equal (add1s '()) '()))

;;; 7.2 use mapcar on the below table to extract a list of social security numbers

(defvar daily-planet
  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))

(defun social-securities ()
  (mapcar #'third daily-planet))

(assert (equal (social-securities)
	       '(123-76-4535 089-52-6787 951-26-1438 355-16-7439)))

;;; 7.3 write a function to apply the zerop predicate to the following input

(defun check-zeroes (ls)
  (mapcar #'zerop ls))

(assert (equal (check-zeroes '(2034 0 -5 -6)) '(nil t nil nil)))

;;; 7.4 repeat 7.3, but this time check if the input is greater than five.
;;; define a predicate greater-than-five-p to help.

(defun greater-than-five-p (x)
  (> x 5))

(defun greater-than-fives (ls)
  (mapcar #'greater-than-five-p ls))

(assert (equal (greater-than-fives '(2034 0 -5 -6)) '(t nil nil nil)))

;;; 7.5 write a lambda expression to subtract seven from a number

(defvar sub-7)

(setf sub-7 (lambda (x) (- x 7)))

(assert (= (funcall sub-7 10) 3))
(assert (= (funcall sub-7 0) -7))

;;; 7.6 write a lambda expression that returns t if its input is t or nil, but nil for any other input.

(defvar t-or-nil)

(setf t-or-nil (lambda (x) (or (equal t x) (null x))))

(assert (funcall t-or-nil t))
(assert (funcall t-or-nil nil))
(assert (not (funcall t-or-nil 0)))
(assert (not (funcall t-or-nil 'true)))

;;; 7.7 write a function that takes a list below and "flips" each element.
;;; Your function should include a lambda expression that flips an individual element.
;;; It should also include an applicative operator to do this to every element of the list.

(defun flips (ls)
  (mapcar #'(lambda (x) (if (equal x 'up) 'down 'up)) ls))

(assert (equal (flips '(up down up up)) '(down up down down)))

;;; 7.8 write a function that takes two inputs, x and k, and returns the first number in the list x that is roughly equal to k (between K-10 and K+10)

(defun roughly-10 (x k)
  (find-if #'(lambda (v) (and (>= v (- k 10))
			      (<= v (+ k 10))))
	   x))

(assert (= (roughly-10 '(110 20 30 -15 8 25) 5) 8))
(assert (= (roughly-10 '(11 20 -3 -15 15 25) -8) -3))

;;; 7.9 write a function find-nested that returns the first element of a list that is itself a non-nil list

(defun find-nested (ls)
  (find-if #'(lambda (v) (not (null v))) ls))

(assert (equal (find-nested '(() () nil (1 2 3) nil)) '(1 2 3)))
(assert (equal (find-nested '(())) nil))

;;; 7.10 a write a table storing the following information in a global variable note-table:
;;; (this is stated in c, but it can't use dotted pairs, although the back of the book solution uses dotted pairs)
;;; C = 1, C-Sharp = 2, D = 3, D-Sharp = 4, E = 5, F = 6, F-Sharp = 7, G = 8, G-Sharp = 9, A = 10, A-Sharp = 11, B = 12

(defvar note-table
  '((c 1)
    (c-sharp 2)
    (d 3)
    (d-sharp 4)
    (e 5)
    (f 6)
    (f-sharp 7)
    (g 8)
    (g-sharp 9)
    (a 10)
    (a-sharp 11)
    (b 12)))

;;; 7.10 b write a function numbers that takes a list of notes as input and returns the corresponding list of numbers

(defun numbers (ls)
  (mapcar #'(lambda (note)
	      (second (assoc note note-table)))
	  ls))

(assert (equal (numbers '(E D C D E E E)) '(5 3 1 3 5 5 5)))

;;; 7.10 c write a function notes that takes a list of number as input and returns the corresponding list of notes.

(defun notes (ls)
  (mapcar #'(lambda (num)
	      (first
	       (find-if (lambda (row)
			  (= (second row) num))
			note-table)))
	  ls))

(assert (equal (notes '(5 3 1 3 5 5 5)) '(E D C D E E E)))

;;; 7.10 d as the above two functions are mutual inverses

(equal (notes (numbers '(A B C))) '(A B C))
(equal (numbers (notes '(1 2 3))) '(1 2 3))

;; what do the following do? 
(notes (notes '(1 2 3))) ; not a num
(numbers (numbers '(A B C)))  ; (nil nil nil)

;;; 7.10 e write a function raise that takes a number n and a list of numbers as input and raises each number in list by n

(defun raise (n ls)
  (mapcar #'(lambda (v) (+ v n)) ls))

(assert (equal (raise 5 '(5 3 1 3 5 5 5)) '(10 8 6 8 10 10 10)))

;;; 7.10 f write a function normalise, that wraps the list of numbers to match a number in the note table
;;; i.e. all numbers should be between 1 and 12

(defun normalise (ls)
  (mapcar #'(lambda (n)
	      (cond
		((> n 12) (- n (* 12 (floor n 12))))
		((< n 1) (+ n 12 (* 12 (floor (* n -1) 12))))
		(t n)))
	  ls))

(assert (equal (normalise '(6 10 13)) '(6 10 1)))
(assert (equal (normalise '(-5 10 13)) '(5 10 1)))
(assert (equal (normalise '(-15 10 13)) '(9 10 1)))

;;; 6.10 g write a function transpose, that takes a number n and a song as input, and returns the song transposed by n half steps
;;; numbers, notes, raise, normalise should be considered

(defun transpose (n ls)
  (notes (normalise (raise n (numbers ls)))))

(assert (equal (transpose 5 '(E D C D E E E)) '(A G F G A A A)))
(assert (equal (transpose 11 '(E D C D E E E)) '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)))
(assert (equal (transpose 12 '(E D C D E E E)) '(E D C D E E E)))
(assert (equal (transpose -1 '(E D C D E E E)) '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)))
