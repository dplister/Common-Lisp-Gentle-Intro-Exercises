;;; 3.5 Using defun, write a definition named half that returns a number that is one-half of its input n. Use defun.

(assert (= (half 6) 3))
(assert (= (half 5) 5/2))
(assert (= (half -2) -1))

;;; 3.5 Using defun, write a definition named cube that returns a number that is to the power of three of its input n. Use defun.

(assert (= (cube 1) 1))
(assert (= (cube 3) 27))
(assert (= (cube 11) 1331))

;;; 3.5 Using defun, write a predicate named onemorep that tests if the first input is exactly one greater than its second input.

(assert (onemorep 2 1))
(assert (not (onemorep 3 1)))
(assert (onemorep -4 -5))

;;; 3.6 Using defun, define a function pythag that takes two inputs, x and y, returns the square root of x^2 + y ^ 2

(assert (= (pythag 3 4) 5.0))
(assert (= (pythag 2 5) 5.3851647))
(assert (= (pythag 12 5) 13))

;;; 3.7 Using defun, define a function miles-per-gallon that takes three inputs:
;;; initial-odometer-reading, final-odometer-reading, gallons-consumed
;;; and computes the number of miles traveled per gallon of gas

(assert (= (miles-per-gallon 110 120 6) 5/3))
(assert (= (miles-per-gallon 110 120 10) 1))
(assert (= (miles-per-gallon 0 15 40) 3/8))

;;; 3.9 evalute the following expressions and determine their results

(cons 5 (list 6 7))

(cons 5 '(list 6 7))

(list 3 'from 9 'give (- 9 3))

(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo)))

(rest '(cons is short for construct))

;;; 3.10 the following expressions all result in errors. Write down the error that occurs,
;;; and how it arose. Correct the expression by changing /only/ the quotes.

(third (the quick brown fox))

(list 2 and 2 is 4)

(+ 1 '(length (list t t t t)))

(cons 'patrick (seymour marvin))

(cons 'patrick (list seymour marvin))

;;; 3.11 define a predicate called longer-than that takes two lists as input and returns T
;;; if the first list is longer than the second

(assert (longer-than '(a b) '(a)))
(assert (not (longer-than '(a b) '(a b c))))
(assert (not (longer-than '(a b c) '(a b c))))

;;; 3.12 write a function addlength that takes a list as input and returns a new list with the length of the input added onto the front of it.
;;; - what is the result of (addlength (addlength '(A B C)))?

(assert (equal (addlength '(MOO GOO GAI PAN)) '(4 MOO GOO GAI PAN)))

;;; 3.13 study the following function and answer the following
;;; - how many arguments does this function require?
;;; - what is the result of (call-up 'fred 'wanda)?

(defun call-up (caller callee)
  (list 'hello callee 'this 'is caller 'calling))

;;; 3.14 what is the result of the following function?

(defun crank-call (caller callee)
  '(hello callee this is caller calling))

;;; 3.15 consider the following function
;;; - the symbol 'word' is used in two ways, what are they?
;;; - what is the result of (scrabble 'aardvark)?
;;; - what is the result of (scrabble 'word)

(defun scrabble (word)
  (list word 'is 'a 'word))

;;; 3.16 based on the function below, what does the function call evaluate to?

(defun stooge (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

;;; 3.19 evaluate the following lists and state the result / error that occurs.

(cons 'grapes '(of wrath))

(list t 'is 'not 'nil)

(first '(list moose goose))

(first (list 'moose 'goose))

(cons 'home ('sweet 'home))

;;; 3.20 Based on the following function, what is the result or error that is produced when evaluating the following expressions?

(defun mystery (x)
  (list (second x) (first x)))

(mystery '(dancing bear))

(mystery 'dancing 'bear)

(mystery '(zowie))

(mystery (list 'first 'second))

;;; 3.21 Identify the problem in each of the following function definitions

(defun speak (x y)
  (list 'all 'x 'is 'y))

(speak 'a 'b)

(defun speak (x) (y)
  (list 'all x 'is y))

(speak 'a 'b)

(defun speak ((x) (y))
  (list all 'x is 'y))

(speak 'a 'b)
