;;; 10.2 rewrite the lemonade stand sell function to use incf instead of setf

(setf *total-glasses* 0)

;; example from book
(defun sell (n)
  (setf *total-glasses* (+ *total-glasses* n))
  (format t "~&Total glasses sold: ~S" *total-glasses*))

(progn
  (setf *total-glasses* 0)
  (sell 5)
  (assert (= *total-glasses* 5)))

;;; 10.3 modify the meet function to keep a count of howm any people have been met more than once

;; example from book
(setf *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*))
	 'we-just-met)
	((member person *friends*)
	 'we-know-each-other)
	(t
	 (push person *friends*)
	 'pleased-to-meet-you)))

(progn
  (setf *friends* nil)
  (meet 'fred)
  (assert (equal *friends* '((fred 0))))
  (meet 'fred)
  (assert (equal *friends* '((fred 1))))
  (meet 'fred)
  (assert (equal *friends* '((fred 2))))
  (meet 'mary)
  (assert (equal *friends* '((mary 0) (fred 2)))))

;;; 10.4 write a function forget that removes a person from the *friends* list
;;; if the person wasn't on the list, the function should complain

(progn
  (setf *friends* nil)
  (meet 'fred)
  (assert (equal *friends* '((fred 0))))
  (forget 'fred)
  (assert (equal *friends* '())))

;;; 10.5 rewrite the following function to use good lisp style

(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is pct 'percent 'of 'max y))

(assert (equal (ugly 20 2) '(average 11.0 is 55.0 percent of max 20)))

(assert (equal (not-ugly 20 2) '(average 11.0 is 55.0 percent of max 20)))

;;; tic-tac-toe reference for exercise 10.8

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
	((equal v 10) "X")
	(t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&  -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&  -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ; horizontal triplets
	(1 4 7) (2 5 8) (3 6 9) ; vertical triplets
	(1 5 9) (3 5 7))) ; diagonal triplets

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun play-one-game()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&You win"))
	  ((board-full-p new-board)
	   (format t "~&Tie game"))
	  (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Yours move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t "~&That space is already occupied")
	   (read-a-legal-move board))
	  (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))
	  
(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&I win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game"))
	  (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))


(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet
	  (find-if #'(lambda (trip)
		       (equal (sum-triplet board trip)
			      target-sum))
		   *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
	       (zerop (nth pos board)))
	   squares))

;;; 10.8 a set *corners* to the four corner positions
;; set *sides* to hold four side squares

;;; 10.8 b write a function block-squeeze-paly that checks the diagonals for an O-X-O pattern and defens by suggesting
;;; a side square as the best move. Should return nil if there is no squeeze play in progress.
;;; should return a list containing a move number and a string explaining the strategy behind the move

(setf squeeze-board '(board 1 0 0 0 10 0 0 0 1))
(setf not-squeeze-board '(board 10 0 0 0 1 0 0 0 1))
(assert (squeeze-p squeeze-board))
(assert (not (squeeze-p not-squeeze-board)))
(assert (equal (block-squeeze-play squeeze-board) (list 2 "squeeze play")))
(assert (not (block-squeeze-play not-squeeze-board)))

;;; 10.8 c write a function block-two-on-one that checks the diagonals for O-O-X or X-O-O pattern and defends by suggesting a corner as the best move
;; should return nil if there is no squeeze play in progress. Should return a list containing a move number and a string explaining strategy.

(setf two-one-board '(board 1 0 0 0 1 0 0 0 10))
(setf not-two-one-board '(board 1 0 0 1 0 0 0 0 10))
(assert (two-on-one-p two-one-board))
(assert (not (two-on-one-p not-two-one-board)))
(assert (equal (block-two-on-one two-one-board) (list 3 "two on one")))
(assert (not (block-two-on-one not-two-one-board)))

;;; 10.8 d modify the choose-best-move function so that it tries these two defensive strategies before choosing a move at random

(assert (equal (choose-best-move squeeze-board) (list 2 "squeeze play")))
(assert (equal (choose-best-move two-one-board) (list 3 "two on one")))

;;; 10.8 e if the computer goes first, then after the opponent's first move there may be an opportunity for the computer to set up a squeeze play or two-on-one situation to trap the opponent
;;; write functions to check the diagonals and suggest an appropriate attack if the opporunity exists
;;; modify choose-best-move to include these offensive strategies

(defvar possible-squeeze '(board 1 0 0 0 10 0 0 0 0))
(defvar possible-two-on-one '(board 1 0 0 0 0 0 0 0 10))

(assert (equal (diagonal-attack possible-squeeze) (list 9 "diagonal attack")))
(assert (equal (diagonal-attack possible-two-on-one) (list 5 "diagonal attack")))
(assert (equal (choose-best-move possible-squeeze) (list 9 "diagonal attack")))
(assert (equal (choose-best-move possible-two-on-one) (list 5 "diagonal attack")))

;;; 10.9 write a destructive function chop that shortens any non-nil list to a list of one element

(let ((a '(1 2 3)))
  (chop a)
  (assert (equal a (list 1))))
(let ((a '(fee fie foo fum)))
  (chop a)
  (assert (equal a (list 'fee))))

;;; 10.10 write a function ntack that destructively tacks a symbol onto a list

(let ((a '(1 2 3)))
  (ntack a 4)
  (assert (equal a (list 1 2 3 4))))
(let ((a '(fee fie foe)))
  (ntack a 'fum)
  (assert (equal a (list 'fee 'fie 'foe 'fum))))

