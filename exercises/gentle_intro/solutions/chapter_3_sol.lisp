;;; 3.5 Using defun, write a definition named half that returns a number that is one-half of its input n. Use defun.
(defun half (n)
  (/ n 2))

(assert (= (half 6) 3))
(assert (= (half 5) 5/2))
(assert (= (half -2) -1))

;;; 3.5 Using defun, write a definition named cube that returns a number that is to the power of three of its input n. Use defun.
(defun cube (n)
  (* n n n)) ; this implementation uses nothing we haven't seen up until this point in the book

(assert (= (cube 1) 1))
(assert (= (cube 3) 27))
(assert (= (cube 11) 1331))

;;; 3.5 Using defun, write a predicate named onemorep that tests if the first input is exactly one greater than its second input.
(defun onemorep (n1 n2)
  (equalp n1 (+ n2 1)))

(assert (onemorep 2 1))
(assert (not (onemorep 3 1)))
(assert (onemorep -4 -5))

;;; 3.6 Using defun, define a function pythag that takes two inputs, x and y, returns the square root of x^2 + y ^ 2

(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))

(assert (= (pythag 3 4) 5.0))
(assert (= (pythag 2 5) 5.3851647))
(assert (= (pythag 12 5) 13))

;;; 3.7 Using defun, define a function miles-per-gallon that takes three inputs:
;;; initial-odometer-reading, final-odometer-reading, gallons-consumed
;;; and computes the number of miles traveled per gallon of gas

(defun miles-per-gallon
  (initial-odometer-reading
   final-odometer-reading
   gallons-consumed)
  (/ (- final-odometer-reading
	initial-odometer-reading)
     gallons-consumed))

(assert (= (miles-per-gallon 110 120 6) 5/3))
(assert (= (miles-per-gallon 110 120 10) 1))
(assert (= (miles-per-gallon 0 15 40) 3/8))

;;; 3.9 evalute the following expressions and determine their results

(cons 5 (list 6 7)) ; (5 6 7)

(cons 5 '(list 6 7)) ; (5 list 6 7)

(list 3 'from 9 'give (- 9 3)) ; (3 from 9 give 6)

(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo))) ; 6

(rest '(cons is short for construct)) ; (is short for construct)

;;; 3.10 the following expressions all result in errors. Write down the error that occurs,
;;; and how it arose. Correct the expression by changing /only/ the quotes.

(third (the quick brown fox)) ; list needs to be escaped
(third '(the quick brown fox)) ; brown

(list 2 and 2 is 4) ; no variables called and / is
(list 2 'and 2 'is 4) ; (2 and 2 is 4)

(+ 1 '(length (list t t t t))) ; length is no calculated as it is escaped by quotes
(+ 1 (length (list t t t t))) ; 5

(cons 'patrick (seymour marvin)) ; seymour marvin are treated as vars
(cons 'patrick '(seymour marvin)) ; (patrick seymour marvin)

(cons 'patrick (list seymour marvin)) ; seymour marvin are treated as vars
(cons 'patrick (list 'seymour 'marvin)) ; (patrick seymour marvin)

;;; 3.11 define a predicate called longer-than that takes two lists as input and returns T
;;; if the first list is longer than the second

(defun longer-than (l1 l2)
  (> (length l1)
     (length l2)))

(assert (longer-than '(a b) '(a)))
(assert (not (longer-than '(a b) '(a b c))))
(assert (not (longer-than '(a b c) '(a b c))))

;;; 3.12 write a function addlength that takes a list as input and returns a new list with the length of the input added onto the front of it.
;;; what is the result of (addlength (addlength '(A B C)))

(defun addlength (ls)
  (cons (length ls) ls))

(addlength (addlength '(A B C))) ; (4 3 A B C)

(assert (equal (addlength '(MOO GOO GAI PAN)) '(4 MOO GOO GAI PAN)))

;;; 3.13 study the following function and answer the following
;;; - how many arguments does this function require?
;;; - what is the result of (call-up 'fred 'wanda)?

(defun call-up (caller callee)
  (list 'hello callee 'this 'is caller 'calling))

(call-up 'fred 'wanda) ; hello wanda this is fred calling

;;; 3.14 what is the result of the following function?

(defun crank-call (caller callee)
  '(hello callee this is caller calling))

(crank-call 'wanda 'fred) ; list is escaped so variables will be ignored

;;; 3.15 consider the following function
;;; - the symbol 'word' is used in two ways, what are they?
;;; - what is the result of (scrabble 'aardvark)?
;;; - what is the result of (scrabble 'word)

(defun scrabble (word)
  (list word 'is 'a 'word))

(scrabble 'aardvark) ; (aardvark is a word)
(scrabble 'word) ; (word is a word)

;;; 3.16 based on the function below, what does the function call evaluate to?

(defun stooge (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

(stooge 'moe 'curly 'larry) ; (moe (moe larry) larry larry)

;;; 3.19 evaluate the following lists and state the result / error that occurs.

(cons 'grapes '(of wrath)) ; (grapes of wrath)

(list t 'is 'not 'nil) ; (t is not nil)

(first '(list moose goose)) ; list

(first (list 'moose 'goose)) ; moose

(cons 'home ('sweet 'home)) ; error, 'sweet is a symbol

;;; 3.20 Based on the following function, what is the result or error that is produced when evaluating the following expressions?

(defun mystery (x)
  (list (second x) (first x)))

(mystery '(dancing bear)) ; (bear dancing)

(mystery 'dancing 'bear) ; incorrect amount of arguments

(mystery '(zowie)) ; (nil zowie) - first, second, etc, return NIL if run off the end of the list

(mystery (list 'first 'second)) ; (second first)

;;; 3.21 Identify the problem in each of the following function definitions

(defun speak (x y)
  (list 'all 'x 'is 'y)) ; evaluate to (all x is y) - the params are escaped

(speak 'a 'b)

(defun speak (x) (y)
  (list 'all x 'is y)) ; expects only one parameter

(speak 'a 'b)

(defun speak ((x) (y))
  (list all 'x is 'y)) ; assumes a symbol instead of a parameter, all and is need to be quoted

(speak 'a 'b)

;;; 3.22 b for each expression, write down what the result will be, or what kind of error it will cause,
;;; then check by running it

(+ 3 5) ; 8

(3 + 5) ; attempt to run 3 as a function

(+ 3 (5 6)) ; attempt to run 5 as a function

(+ 3 (* 5 6)) ; 33

'(morning noon night) ; (morning noon night)

('morning 'noon 'night) ; missing list call - morning treated as function

(list 'morning 'noon 'night) ; (morning noon night)

(car nil) ; nil

(+ 3 foo) ; foo not a number, treated as variable

(+ 3 'foo) ; foo not a number, treated as symbol

;;; 3.22 c based on the following call, define myfun

(myfun 'alpha 'beta) ; ((alpha) beta)

(defun myfun (a b)
  (list (list a) b))

;;; 3.22 d write a predicate firstp that returns t if its first argument (a symbol) is equal to the
;;; first element of its second argument (a list).

(defun firstp (v ls)
  (equal v (car ls)))

(assert (firstp 'foo '(foo bar baz)))
(assert (not (firstp 'boing '(foo bar baz))))

;;; 3.22 e write a function mid-add1 that adds 1 to the middle element of a three-element list

(defun mid-add1 (ls)
  (list (first ls)
	(+ (second ls) 1)
	(third ls)))

(assert (equal (mid-add1 '(take 2 cookies)) '(take 3 cookies)))

;;; 3.22 f write a function f-to-c that converts a temperature from fahrenheit to celsius
;;; the formula is: Celsius temperature = [5 x (Fahrenheit temp - 32)]/9

(defun f-to-c (fahrenheit)
  (/ (* 5 (- fahrenheit 32)) 9))

(assert (= (f-to-c 14) -10))
(assert (= (f-to-c 32) 0))
(assert (= (f-to-c 59) 15))

;;; 3.22 g what is wrong with this function? What does (foo 5) do?

(defun foo (x)
  (+ 1 (zerop x))) 

(foo t) ; attempts to add a number to a predicate (t / nil) response

;;; 3.25 what do the following expressions evaluate to?

(list 'cons t nil) ; (cons t nil)

(eval (list 'cons t nil)) ; (t)

(eval (eval (list 'cons t nil))) ; error; attempting to eval (t)

(apply #'cons '(t nil))) ; (t)

(eval nil) ; nil

(list 'eval nil) ; (eval nil)

(eval (list 'eval nil)) ; nil
