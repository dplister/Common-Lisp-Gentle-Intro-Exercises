;;; 5.1 rewrite poor-style to create a new local variable q using let, instead of using setf to change p. Call your new function good-style.

(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(assert (equal (poor-style 8) '(result is 13)))

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))

(assert (equal (good-style 8) '(result is 13)))

;;; 5.6 dice exerice. Start with a function to throw one die and end up with a program to play craps.
;;; - include documentation strings for each function

;;; 5.6 a write a function throw-die that returns a random number from 1-6 inclusive.

(defun throw-die ()
  "returns a value reflecting a 1d6 roll"
  (+ (random 6) 1))

(dotimes (n 10)
  (let ((result (throw-die)))
    (assert (>= result 1))
    (assert (<= result 6))))

;;; 5.6 b write a function throw-dice that throws two dice and returns their result as a list.

(defun throw-dice ()
  "returns the list of two die rolls"
  (list (throw-die) (throw-die)))

(assert (= (length (throw-dice)) 2))
(assert (numberp (first (throw-dice))))
(assert (numberp (second (throw-dice))))

;;; 5.7 c throwing two ones is called "snake eyes"; two sixes is called "box-cars".
;;; write a predicate snake-eyes-p that takes a throw as input and returns t if equal to (1 1)
;;; write a predicate boxcars-p that takes a throw as input and returns t if equal to (6 6)

(defun snake-eyes-p (ls)
  "identifies if the two die rolled are snake-eyes (two ones)"
  (and (= (first ls) 1) (= (second ls) 1)))

(assert (snake-eyes-p '(1 1)))
(assert (not (snake-eyes-p '(1 2))))
(assert (not (snake-eyes-p '(2 1))))

(defun boxcars-p (ls)
  "identifies if the two die rolled are boxcars (two sixes)"
  (and (= (first ls) 6) (= (second ls) 6)))

(assert (boxcars-p '(6 6)))
(assert (not (boxcars-p '(1 6))))
(assert (not (boxcars-p '(6 1))))

;;; 5.7 d the first throw in craps can result in an instant win or loss.
;;; - write a predicate instant-win-p that identifies a throw of 7 or 11
;;; - write a predicate instant-loss-p that identifies a throw of 2, 3 or 12

(defun instant-win-p (ls)
  "identifies if the total result of input dice throws is 7 or 11"
  (let ((result (throw-value ls)))
    (or (= result 7)
	(= result 11))))

(assert (instant-win-p '(3 4)))
(assert (instant-win-p '(5 6)))
(assert (not (instant-win-p '(1 2))))
(assert (not (instant-win-p '(6 6))))

(defun instant-loss-p (ls)
  "identifies if the total result of input dice throws is 2, 3 or 12"
  (let ((result (throw-value ls)))
    (or (= result 2)
	(= result 3)
	(= result 12))))

(assert (instant-loss-p '(1 1)))
(assert (instant-loss-p '(1 2)))
(assert (instant-loss-p '(6 6)))
(assert (not (instant-loss-p '(6 5))))
(assert (not (instant-loss-p '(3 3))))

;; this is a useful function I picked up from the official answer

(defun throw-value (ls)
  (apply #'+ ls))

;;; 5.7 e write a function say-throw that takes a throw as input and returns either the sum of the dice or symbol 'snake-eyes or 'boxcars.

(defun say-throw (ls)
  "identifies the result of the throw, either a score or named result"
  (cond
    ((snake-eyes-p ls) 'snake-eyes)
    ((boxcars-p ls) 'boxcars)
    (t (throw-value ls))))

(assert (equal (say-throw '(3 6)) 9))
(assert (equal (say-throw '(1 1)) 'snake-eyes))
(assert (equal (say-throw '(6 6)) 'boxcars))

;;; 5.7 f write a function craps that produces the following behaviour (look at assertions)
;;; - deviates from book by allowing you to set the value passed in

(defun craps (ls)
  "resolves a throw in craps"
  (append
   (throw-format ls)
   ;; resolve the throw
   (let ((result (say-throw ls)))
     (cond
       ((instant-win-p ls) (list result '-- 'you 'win))
       ((instant-loss-p ls) (list result '-- 'you 'lose))
       (t (list 'your 'point 'is result))))))

;; re-usable function that will be helpful for the next function

(defun throw-format (ls)
  "outputs the result of the throw as a list description"
  (list 'throw (first ls) 'and (second ls) '--))

(assert (equal (craps '(1 1)) '(throw 1 and 1 -- snake-eyes -- you lose)))
(assert (equal (craps '(3 4)) '(throw 3 and 4 -- 7 -- you win)))
(assert (equal (craps '(2 4)) '(throw 2 and 4 -- your point is 6)))

;;; 5.7 g write a function try-for-point that throws a pair of dice
;;; if your roll totals the number passed in as input, you win
;;; if you roll a 7, you lose
;;; any other number, you are asked to throw again
;;; - deviates from book by allowing you to set the value passed in

(defun try-for-point (p ls)
  "resolves a second roll in craps"
  (let ((result (throw-value ls)))
    (append
     (throw-format ls)
     (list result '--)
     (cond
       ((= result 7) '(you lose))
       ((= result p) '(you win))
       (t '(throw again))))))

(assert (equal (try-for-point 6 '(3 5)) '(throw 3 and 5 -- 8 -- throw again)))
(assert (equal (try-for-point 6 '(5 1)) '(throw 5 and 1 -- 6 -- you win)))
(assert (equal (try-for-point 9 '(6 1)) '(throw 6 and 1 -- 7 -- you lose)))
