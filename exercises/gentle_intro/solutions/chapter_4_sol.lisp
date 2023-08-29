;;; 4.1 write a function make-even that makes an odd number even, and returns number as-is if already even

(defun make-even (num)
  (if (oddp num) (+ num 1) num))

(assert (= (make-even 3) 4))
(assert (= (make-even 4) 4))

;;; 4.2 write a function further that makes a positive number larger by adding one to it, and a negative number smaller by subtracting one from it. What does your function do if the given number is 0?

(defun further (num)
  (if (< num 0) (- num 1) (+ num 1)))

(assert (= (further -1) -2))
(assert (= (further 1) 2))
(assert (= (further 0) 1))

;;; 4.3 implement my-not, a function that works like the not primitive (without using not)

(defun my-not (test)
  (if test nil t))

(assert (my-not nil))
(assert (not (my-not t)))

;;; 4.4 write a function ordered that takes two numbers as input and returns them as a list in ascending order

(defun ordered (n1 n2)
  (if (< n1 n2) (list n1 n2) (list n2 n1)))

(assert (equal (ordered 1 2) (list 1 2)))
(assert (equal (ordered 2 1) (list 1 2)))

;;; 4.6 write a version of my-abs using cond instead of if
;;; (defun my-abs (x) (if (< x 0) (- x) x))

(defun my-abs (x)
  (cond
    ((< x 0) (- x))
    (t x)))

(assert (= (my-abs -5) 5))
(assert (= (my-abs 5) 5))

;;; 4.7 for each cond expression, identify if the parens are incorrect, if so, explain where the error lies

(setf x 'asd)

(cond (symbolp x) 'symbol
      (t 'not-a-symbol)) ;; missing paren around first clause

(cond ((symbolp x) 'symbol) 
      (t 'not-a-symbol)) ;; works

(cond ((symbolp x) ('symbol))
      (t 'not-a-symbol)) ;; excess parens around first clause result

(cond ((symbolp x) 'symbol)
      ((t 'not-a-symbol))) ;; excess parens around last clause
       
;;; 4.8 write emphasise3 which is similar to the below but adds the symbol very onto the list if it doesn't know how to emphasise the input
;;; what does an input of '(very long day) produce?

(defun emphasise2 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t x)))

(defun emphasise3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t (cons 'very x))))

(assert (equal (emphasise3 '(good day)) '(great day)))
(assert (equal (emphasise3 '(bad day)) '(awful day)))
(assert (equal (emphasise3 '(long day)) '(very long day)))

;;; 4.9 What is wrong with the following function?
;;; try inputs 3, 4, -2. Rewrite so it works correctly.

(defun make-odd (x)
  (cond (t x)
	((not (oddp x)) (+ x 1)))) ; first clause should be last as it always evaluates to true

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
	(t x)))

(assert (= (make-odd 3) 3))
(assert (= (make-odd 4) 5))
(assert (= (make-odd -2) -1))

;;; 4.10 write a function constrain that takes three inputs (x, max, min). if x is outside of max or min, it is bound to the max or min.

(defun constrain (x min max)
  (cond
    ((< x min) min)
    ((> x max) max)
    (t x)))

(assert (= (constrain 3 -50 50) 3))
(assert (= (constrain 92 -50 50) 50))
(assert (= (constrain -92 -50 50) -50))

;;; 4.11 write a function firstzero that takes a list of three numbers as input and returns a word (first, second, third, none) indicating where the first zero appears in the list

(defun firstzero (ls)
  (cond
    ((= (first ls) 0) 'first)
    ((= (second ls) 0) 'second)
    ((= (third ls) 0) 'third)
    (t 'none)))

(assert (equal (firstzero '(0 3 4)) 'first))
(assert (equal (firstzero '(3 0 4)) 'second))
(assert (equal (firstzero '(3 4 0)) 'third))
(assert (equal (firstzero '(3 4 5)) 'none))

;;; 4.12 write a function cycle that cyclically counts from 1 to 99.

(defun cycle (x)
  (cond
    ((>= x 99) 1)
    (t (+ x 1))))

(assert (= (cycle 1) 2))
(assert (= (cycle 2) 3))
(assert (= (cycle 99) 1))

;;; 4.13 write a function howcompute that takes three numbers as input and figures out what operation would produce the third value from the first two

(defun howcompute (a b result)
  (cond
    ((= (+ a b) result) 'sum-of)
    ((= (* a b) result) 'product-of)
    (t 'beats-me)))

(assert (equal (howcompute 3 4 7) 'sum-of))
(assert (equal (howcompute 3 4 12) 'product-of))
(assert (equal (howcompute 3 4 99) 'beats-me))
