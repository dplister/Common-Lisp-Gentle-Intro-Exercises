;;; note: the auto indentation in emacs broke for this file for unknown reasons
;;; there are fewer tests, as the exercises are organised around printing to screen

;;; 9.1 write a function that prints a multi-line quote

;;; 9.2 write a recursive draw-line function that draws a line of specified length by doing the following the correct number of times

(format t "*")

;;; 9.3 write a recursive function draw-box that calls draw-line repeatedly to draw a box of specified dimensions

;;; 9.4 write a recursive function ninety-nine-bottles that sings the song.
;; take an n parameter that starts at n bottles and counts to 0

;;; 9.5 implement print-board, printing a tic-tac-toe board

;;; 9.6 write a function to compute an hourly workers gross pay given as hourly wage in dollars and the number of hours he or she worked.

;;; 9.7 write cookie-monster function keeps reading data from the terminal until it reads the symbol cookie

;;; 9.10 a write a recursive function space-over that takes a number n as input and moves the cursor to the right by printing n spaces one at a time
;;; should print "Error!" if n is negative

(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

;;; 9.10 b write a function plot-one-point that takes two inputs, plotting-string and y-val, prints plotting-string (without the quotes) in column y-val, and the moves to a new line.
;;; leftmost column is numbered zero

;;; 9.10 c write a function plot-points that takes a string and a list of y values as input and plots them

(plot-points "< >" '(4 6 8 10 8 6 4))

;;; 9.10 d write function generate that takes two numbers M and N as input and returns a list of the integers from M and N

(assert (equal (generate -3 3) '(-3 -2 -1 0 1 2 3)))

;;; 9.10 e write make-graph function, make-graph should prompt for the values of func, start, end, plotting-string and then graph the funciton

(make-graph (lambda (v) (if (oddp v) (* v 2 -1) (* v 2))) 1 10 "*")

;;; 9.10 f define the square function and graph it over the range -7 to 7, use your name as the plotting symbol

(make-graph #'square -7 7 "Me")

;;; 9.11 write a function dot-prin1 that takes a list as input and prints it in dot notation

(dot-prin1 '(A B))
(dot-prin1 '(A (B) C))	    
(dot-prin1 '((((A)))))

;;; 9.12 try the following

(dot-prin1 '(A . (B . C)))

;;; 9.15 write hybrid-prin
;; if cdr of cons cell is a list, continue printing list
;; if cdr part is nil, print right paren
;; if cdr part is something else such as a symbole, print a dot, the symbol, and a right paren

(hybrid-prin '(A . B))
(hybrid-prin '(A B))
(hybrid-prin '(A B C . D))
(hybrid-prin '((A) B C . D)) 
     
     
