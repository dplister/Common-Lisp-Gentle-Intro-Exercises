;;; 4.1 write a function make-even that makes an odd number even, and returns number as-is if already even

(assert (= (make-even 3) 4))
(assert (= (make-even 4) 4))

;;; 4.2 write a function further that makes a positive number larger by adding one to it, and a negative number smaller by subtracting one from it. What does your function do if the given number is 0?

(assert (= (further -1) -2))
(assert (= (further 1) 2))
(assert (= (further 0) 1))

;;; 4.3 implement my-not, a function that works like the not primitive (without using not)

(assert (my-not nil))
(assert (not (my-not t)))

;;; 4.4 write a function ordered that takes two numbers as input and returns them as a list in ascending order

(assert (equal (ordered 1 2) (list 1 2)))
(assert (equal (ordered 2 1) (list 1 2)))

;;; 4.6 write a version of my-abs using cond instead of if
;;; (defun my-abs (x) (if (< x 0) (- x) x))

(assert (= (my-abs -5) 5))
(assert (= (my-abs 5) 5))

;;; 4.7 for each cond expression, identify if the parens are incorrect, if so, explain where the error lies

(setf x 'asd) ; change this if you want to test the below

(cond (symbolp x) 'symbol
      (t 'not-a-symbol))

(cond ((symbolp x) 'symbol) 
      (t 'not-a-symbol))

(cond ((symbolp x) ('symbol))
      (t 'not-a-symbol))

(cond ((symbolp x) 'symbol)
      ((t 'not-a-symbol)))
       
;;; 4.8 write emphasise3 which is similar to the below but adds the symbol 'very onto the list if it doesn't know how to emphasise the input
;;; what does an input of '(very long day) produce?

(defun emphasise2 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t x)))

(assert (equal (emphasise3 '(good day)) '(great day)))
(assert (equal (emphasise3 '(bad day)) '(awful day)))
(assert (equal (emphasise3 '(long day)) '(very long day)))

;;; 4.9 What is wrong with the following function?
;;; try inputs 3, 4, -2. Rewrite so it works correctly.

(defun make-odd (x)
  (cond (t x)
	((not (oddp x)) (+ x 1)))) 

(assert (= (make-odd 3) 3))
(assert (= (make-odd 4) 5))
(assert (= (make-odd -2) -1))

;;; 4.10 write a function constrain that takes three inputs (x, max, min). if x is outside of max or min, it is bound to the max or min.

(assert (= (constrain 3 -50 50) 3))
(assert (= (constrain 92 -50 50) 50))
(assert (= (constrain -92 -50 50) -50))

;;; 4.11 write a function firstzero that takes a list of three numbers as input and returns a word (first, second, third, none) indicating where the first zero appears in the list

(assert (equal (firstzero '(0 3 4)) 'first))
(assert (equal (firstzero '(3 0 4)) 'second))
(assert (equal (firstzero '(3 4 0)) 'third))
(assert (equal (firstzero '(3 4 5)) 'none))

;;; 4.12 write a function cycle that cyclically counts from 1 to 99.

(assert (= (cycle 1) 2))
(assert (= (cycle 2) 3))
(assert (= (cycle 99) 1))

;;; 4.13 write a function howcompute that takes three numbers as input and figures out what operation would produce the third value from the first two

(assert (equal (howcompute 3 4 7) 'sum-of))
(assert (equal (howcompute 3 4 12) 'product-of))
(assert (equal (howcompute 3 4 99) 'beats-me))

;;; 4.14 what results do the following expressions produce?

(and 'fee 'fie' 'foe)

(or 'fee 'fie 'foe)

(or nil 'foe nil)

(and 'fee 'fie nil)

(and (equal 'abc 'abc) 'yes)

(or (equal 'abc 'abc) 'yes)

;;; 4.15 write a predicate called geq that returns t if its first input is greater than or equal to its second input

(assert (not (geq 1 2)))
(assert (geq 1 1))
(assert (geq 2 1))

;;; 4.16 write a function that:
;;; - squares a number if it is odd and positive
;;; - doubles a number if it is odd and negative
;;; - otherwise, divides the number by 2

(assert (= (squadiv 3) 9))
(assert (= (squadiv -3) -6))
(assert (= (squadiv 0) 0))
(assert (= (squadiv 4) 2))

;;; 4.17 write a predicate that returns t if
;;; - first input is boy/girl and second input is child
;;; - first input is man/woman and second input is adult

(assert (ident 'man 'adult))
(assert (ident 'boy 'child))
(assert (ident 'woman 'adult))
(assert (ident 'girl 'child))
(assert (not (ident 'man 'child)))
(assert (not (ident 'girl 'adult)))
  
;;; 4.18 write a referee for rock-scissors paper, takes two player inputs and returns the
;;; result of the play

(assert (equal (rsp 'rock 'scissors) 'first-wins))
(assert (equal (rsp 'rock 'paper) 'second-wins))
(assert (equal (rsp 'scissors 'scissors) 'tie))

;;; 4.19 show how to write the expression (AND x y z w) using cond instead of and
;;; then show how to write it using nested ifs

(assert (= (and-cond 1 2 3 4) 4))
(assert (not (and-cond 1 2 nil 4)))

(assert (= (and-if 1 2 3 4) 4))
(assert (not (and-if 1 2 nil 4)))

;;; 4.20 write a version of the compare function using if instead of cond
;;; - also write a version using and and or

(defun example-compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) 'first-is-bigger)))

(assert (equal (compare-if 1 1) 'numbers-are-the-same))
(assert (equal (compare-if 1 2) 'first-is-smaller))
(assert (equal (compare-if 2 1) 'first-is-bigger))

(assert (equal (compare-and-or 1 1) 'numbers-are-the-same))
(assert (equal (compare-and-or 1 2) 'first-is-smaller))
(assert (equal (compare-and-or 2 1) 'first-is-bigger))

;;; 4.21 write version of the gtest function using if and cond

(defun example-gtest (x y)
  (or (> x y)
      (zerop x)
      (zerop y)))

(assert (gtest-if 2 1))
(assert (gtest-if 0 1))
(assert (gtest-if -1 0))
(assert (not (gtest-if 1 2)))

(assert (gtest-cond 2 1))
(assert (gtest-cond 0 1))
(assert (gtest-cond -1 0))
(assert (not (gtest-cond 1 2)))

;;; 4.22 use cond to write a predicate boilingp that takes two inputs, temp and scale
;;; and returns t if the temperature is above the boiling point of water on the specified scale
;;; write versions using if and and/or instead of cond

(defun boilingp (temp scale)
  (cond
    ((equal scale 'fahrenheit) (>= temp 212))
    ((equal scale 'celsius) (>= temp 100))))

; min boiling points
(assert (boilingp 212 'fahrenheit))
(assert (boilingp 100 'celsius))

(assert (not (boilingp 1 'fahrenheit)))
(assert (not (boilingp 1 'celsius)))
(assert (boilingp 300 'fahrenheit))
(assert (boilingp 200 'celsius))

; min boiling points
(assert (boilingp-if 212 'fahrenheit))
(assert (boilingp-if 100 'celsius))

(assert (not (boilingp-if 1 'fahrenheit)))
(assert (not (boilingp-if 1 'celsius)))
(assert (boilingp-if 300 'fahrenheit))
(assert (boilingp-if 200 'celsius))

; min boiling points
(assert (boilingp-and-or 212 'fahrenheit))
(assert (boilingp-and-or 100 'celsius))

(assert (not (boilingp-and-or 1 'fahrenheit)))
(assert (not (boilingp-and-or 1 'celsius)))
(assert (boilingp-and-or 300 'fahrenheit))
(assert (boilingp-and-or 200 'celsius))
