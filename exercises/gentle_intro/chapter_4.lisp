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
