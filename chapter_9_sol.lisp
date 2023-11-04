;;; note: the auto indentation in emacs broke for this file for unknown reasons

;;; 9.1 write a function that prints a multi-line quote

(defun quoted ()
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")
  (format t "~&but there are no old bold pilots."))

;;; 9.2 write a recursive draw-line function that draws a line of specified length by doing the following the correct number of times

(format t "*")

(defun draw-line (n)
  (cond
    ((<= n 0) nil)
    (t
     (format t "*")
     (draw-line (- n 1)))))

;;; 9.3 write a recursive function draw-box that calls draw-line repeatedly to draw a box of specified dimensions

(defun draw-box (w h)
  (cond
    ((<= h 0)
     nil)
    (t
     (format t "~&")
     (draw-line w)
     (draw-box w (- h 1)))))

;;; 9.4 write a recursive function ninety-nine-bottles that sings the song.
;; take an n parameter that starts at n bottles and counts to 0

(defun ninety-nine-bottles (n)
  (cond
    ((= n 0)
     (format t "~&No more bottles of beer on the wall."))
    ((= n 1)
     (format t "~&1 bottle of beer on the wall.")
     (ninety-nine-bottles (- n 1)))
    (t
     (format t "~&~A bottles of beer on the wall." n)
     (ninety-nine-bottles (- n 1)))))

;;; 9.5 implement print-board, printing a tic-tac-toe board

(defvar tokens
  '((x . "X")
    (o . "O")
    (nil . " ")))

(defun print-separator ()
  (format t "~&-----------"))

(defun print-board (ls)
  (let ((tks (sublis tokens ls)))
    (loop for (left mid right) on tks by 'cdddr
	  for rnum in '(1 2 3)
	  do
	     (when (> rnum 1)
	       (print-separator))
	     (format t "~& ~A | ~A | ~A" left mid right)))))

;;; 9.6 write a function to compute an hourly workers gross pay given as hourly wage in dollars and the number of hours he or she worked.

(defun gross-pay ()
  (format t "~&Enter your hourly wage:  ")
  (let ((wage (read)))
    (format t "~&Enter the hours worked:  ")
    (let* ((hours (read))
	   (total (* wage hours)))
      (format t "~&Total wage: ~A" total))))

;;; 9.7 write cookie-monster function keeps reading data from the terminal until it reads the symbol cookie

(defun cookie-monster ()
  (format t "~&Give me cookie!")
  (format t "~&Cookie? ")
  (let ((response (read)))
    (cond
      ((string-equal response "cookie")
       (format t "~&Thank you! ...Munch munch munch..."))
      (t
       (format t "~&No want ~A" response)
       (cookie-monster)))))

;;; 9.10 a write a recursive function space-over that takes a number n as input and moves the cursor to the right by printing n spaces one at a time
;;; should print "Error!" if n is negative

(defun space-over (n)
 (cond
    ((< n 0) (format t "Error!"))
    ((= n 0) nil)
    (t
     (format t " ")
     (space-over (- n 1)))))

(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

;;; 9.10 b write a function plot-one-point that takes two inputs, plotting-string and y-val, prints plotting-string (without the quotes) in column y-val, and the moves to a new line.
;;; leftmost column is numbered zero

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))

;;; 9.10 c write a function plot-points that takes a string and a list of y values as input and plots them

(defun plot-points (plotting-string y-vals)
  (cond
    ((null y-vals) nil)
    (t
     (plot-one-point plotting-string (first y-vals))
     (plot-points plotting-string (rest y-vals))))))

(plot-points "< >" '(4 6 8 10 8 6 4))

;;; 9.10 d write function generate that takes two numbers M and N as input and returns a list of the integers from M and N

(defun generate (m n)
(loop for num from m upto n
collect num))

(assert (equal (generate -3 3) '(-3 -2 -1 0 1 2 3)))

;;; 9.10 e write make-graph function, make-graph should prompt for the values of func, start, end, plotting-string and then graph the funciton

(defun make-graph (f start end plotting-string)
(plot-points plotting-string
(make-positive (mapcar f (generate start end)))))

(defun make-positive (ls)
"shifts all numbers so that ls contains only positive numbers"
(let* ((mn (reduce #'min ls))
      (inc (* mn -1)))
(if (< mn 0)
    (mapcar #'(lambda (v) (+ v inc)) ls)
    ls)))

(make-graph (lambda (v) (if (oddp v) (* v 2 -1) (* v 2))) 1 10 "*")

;;; 9.10 f define the square function and graph it over the range -7 to 7, use your name as the plotting symbol

(defun square (n)
  (* n n))

(make-graph #'square -7 7 "Dan")

;;; 9.11 write a function dot-prin1 that takes a list as input and prints it in dot notation

(defun dot-prin1 (ls)
  (cond
    ((null ls)
     (format t "nil"))
    ((symbolp ls)
     (format t "~A" ls))
    ((consp ls)
     (format t "(")
     (dot-prin1 (car ls))
     (format t " . ")
     (dot-prin1 (cdr ls))
     (format t ")"))))

(dot-prin1 '(A B))
(dot-prin1 '(A (B) C))	    
(dot-prin1 '((((A)))))

;;; 9.12 try the following

(dot-prin1 '(A . (B . C)))

;;; 9.15 write hybrid-prin
;; if cdr of cons cell is a list, continue printing list
;; if cdr part is nil, print right paren
;; if cdr part is something else such as a symbole, print a dot, the symbol, and a right paren

(defun hybrid-prin (ls)
  (cond
    ((null ls)
     (format t "nil"))
    ((symbolp ls)
     (format t "~A" ls))
    ((consp ls)
     (format t "(")
     (hybrid-prin-ls ls)
     (format t ")"))))

(defun hybrid-prin-ls (ls)
  (cond
    ((null ls) nil)
    ((null (cdr ls))
     (hybrid-prin (first ls)))
    ((symbolp (cdr ls))
     (hybrid-prin (first ls))
     (format t " . ")
     (hybrid-prin (cdr ls)))
    (t
     (hybrid-prin (first ls))
     (format t " ")
     (hybrid-prin-ls (rest ls)))))

(hybrid-prin '(A . B))
(hybrid-prin '(A B))
(hybrid-prin '(A B C . D))
(hybrid-prin '((A) B C . D)) 
     
     
