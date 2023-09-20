;;; 8.4 write a function called laugh that takes a number as input and returns al ist of that many has

;; a scaffold is provided
;; (defun laugh (n)
;;  (cond (A B)
;;	(t (cons 'ha C))))

(defun laugh (n)
  (cond ((<= n 0) '())
	(t (cons 'ha (laugh (- n 1))))))

(assert (equal (laugh 3) '(ha ha ha)))
(assert (equal (laugh 0) '()))
(assert (equal (laugh -1) '()))

;;; 8.5 write a recursive function add-up to add up all the numbers in a list

(defun add-up (ls)
  "sums numbers in list"
  (cond ((null ls) 0)
	(t (+ (first ls) (add-up (rest ls))))))

(assert (= (add-up '(2 3 7)) 12))
(assert (= (add-up '()) 0))

;;; 8.6 write alloddp, a recursive function that returns t if all the numbers in a list are odd

(defun alloddp (ls)
  "determines if all numbers are odd in list"
  (cond
    ((null ls) t)
    ((not (oddp (first ls))) nil)
    (t (alloddp (rest ls)))))

(assert (alloddp '(1 3 7)))
(assert (alloddp '()))
(assert (not (alloddp '(1 4 5))))

;;; 8.7 write rec-member, a recursive version of member

(defun rec-member (item list)
  (cond
    ((null list) nil)
    ((equal (first list) item) list)
    (t (rec-member item (rest list)))))

(assert (equal (rec-member 'b '(a b c)) '(b c)))
(assert (equal (rec-member 'd '(a b c)) nil))
(assert (equal (rec-member 'd '()) nil))

;;; 8.8 write rec-assoc, a recursive version of assoc

(defun rec-assoc (item alist)
  (cond
    ((null alist) nil)
    ((equal (first (first alist)) item) (first alist))
    (t (rec-assoc item (rest alist)))))

(assert (equal (rec-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (not (rec-assoc 'd '((a 1) (b 2) (c 3)))))
(assert (not (rec-assoc 'd '())))

;;; 8.9 write rec-nth, a recursive version of nth

(defun rec-nth (n ls)
  (cond
    ((null ls) nil)
    ((<= n 0) (first ls))
    (t (rec-nth (- n 1) (rest ls)))))

(assert (equal (rec-nth 2 '(a b c)) 'c))
(assert (equal (rec-nth 2 '(a b)) nil))
(assert (equal (rec-nth 0 '(a b)) 'a))
(assert (equal (rec-nth -1 '(a b)) 'a))
