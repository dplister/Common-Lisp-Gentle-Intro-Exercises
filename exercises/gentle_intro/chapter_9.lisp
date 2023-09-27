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
    
