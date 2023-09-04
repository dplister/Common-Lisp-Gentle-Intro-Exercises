;;; 5.1 rewrite poor-style to create a new local variable q using let, instead of using setf to change p. Call your new function good-style.

(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(assert (equal (poor-style 8) '(result is 13)))

(assert (equal (good-style 8) '(result is 13)))

;;; 5.6 dice exercise. Start with a function to throw one die and end up with a program to play craps.
;;; - include documentation strings for each function

;;; 5.6 a write a function throw-die that returns a random number from 1-6 inclusive.

(dotimes (n 10)
  (let ((result (throw-die)))
    (assert (>= result 1))
    (assert (<= result 6))))

;;; 5.6 b write a function throw-dice that throws two dice and returns their result as a list.

(assert (= (length (throw-dice)) 2))
(assert (numberp (first (throw-dice))))
(assert (numberp (second (throw-dice))))

;;; 5.7 c throwing two ones is called "snake eyes"; two sixes is called "box-cars".
;;; write a predicate snake-eyes-p that takes a throw as input and returns t if equal to (1 1)
;;; write a predicate boxcars-p that takes a throw as input and returns t if equal to (6 6)

(assert (snake-eyes-p '(1 1)))
(assert (not (snake-eyes-p '(1 2))))
(assert (not (snake-eyes-p '(2 1))))

(assert (boxcars-p '(6 6)))
(assert (not (boxcars-p '(1 6))))
(assert (not (boxcars-p '(6 1))))

;;; 5.7 d the first throw in craps can result in an instant win or loss.
;;; - write a predicate instant-win-p that identifies a throw of 7 or 11
;;; - write a predicate instant-loss-p that identifies a throw of 2, 3 or 12

(assert (instant-win-p '(3 4)))
(assert (instant-win-p '(5 6)))
(assert (not (instant-win-p '(1 2))))
(assert (not (instant-win-p '(6 6))))

(assert (instant-loss-p '(1 1)))
(assert (instant-loss-p '(1 2)))
(assert (instant-loss-p '(6 6)))
(assert (not (instant-loss-p '(6 5))))
(assert (not (instant-loss-p '(3 3))))

;; this is a useful function I picked up from the official answer

(defun throw-value (ls)
  (apply #'+ ls))

;;; 5.7 e write a function say-throw that takes a throw as input and returns either the sum of the dice or symbol 'snake-eyes or 'boxcars.

(assert (equal (say-throw '(3 6)) 9))
(assert (equal (say-throw '(1 1)) 'snake-eyes))
(assert (equal (say-throw '(6 6)) 'boxcars))

;;; 5.7 f write a function craps that produces the following behaviour (look at assertions)
;;; - deviates from book by allowing you to set the value passed in

(assert (equal (craps '(1 1)) '(throw 1 and 1 -- snake-eyes -- you lose)))
(assert (equal (craps '(3 4)) '(throw 3 and 4 -- 7 -- you win)))
(assert (equal (craps '(2 4)) '(throw 2 and 4 -- your point is 6)))

;;; 5.7 g write a function try-for-point that throws a pair of dice
;;; if your roll totals the number passed in as input, you win
;;; if you roll a 7, you lose
;;; any other number, you are asked to throw again
;;; - deviates from book by allowing you to set the value passed in

(assert (equal (try-for-point 6 '(3 5)) '(throw 3 and 5 -- 8 -- throw again)))
(assert (equal (try-for-point 6 '(5 1)) '(throw 5 and 1 -- 6 -- you win)))
(assert (equal (try-for-point 9 '(6 1)) '(throw 6 and 1 -- 7 -- you lose)))
