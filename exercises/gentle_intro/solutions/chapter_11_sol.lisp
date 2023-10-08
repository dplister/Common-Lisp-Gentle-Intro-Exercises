;;; 11.1 write an iterative version of the member function, called it-member.
;;; it does not need to return a sublist

(defun it-member (item ls)
  (dolist (x ls)
    (when (equal x item)
      (return t))))

(assert (it-member 'b '(a b c)))
(assert (not (it-member 'd '(a b c))))

;;; 11.2 write an iterative version of assoc, it-assoc

(defun it-assoc (item ls)
  (dolist (x ls)
    (when (equal (first x) item)
      (return x))))

(assert (equal (it-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (not (it-assoc 'd '((a 1) (b 2) (c 3)))))

;;; 11.3 write a recursive version of check-all-odd, including messages from example

;; example from book
(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

(assert (check-all-odd '(1 3 5)))
(assert (not (check-all-odd '(1 3 4 5))))

(defun check-all-odd-rec (list-of-numbers)
  (cond
    ((null list-of-numbers) t)
    (t
     (format t "~&Checking ~S..." (first list-of-numbers))
     (if (evenp (first list-of-numbers)) nil
	 (check-all-odd-rec (rest list-of-numbers))))))

(assert (check-all-odd-rec '(1 3 5)))
(assert (not (check-all-odd-rec '(1 3 4 5))))

;;; 11.4 write an iterative version of length, called it-length

(defun it-length (ls)
  (let ((total 0))
    (dolist (el ls total)
      (incf total))))

(assert (= (it-length nil) 0))
(assert (= (it-length '(a b c)) 3))

;;; 11.5 write an iterative version of nth, called it-nth

(defun it-nth (n ls)
  (car
   (let ((res ls))
     (dotimes (index n res)
       (setf res (cdr res))))))

(assert (equal (it-nth 2 '(a b c)) 'c))
(assert (not (it-nth 5 '(a b c))))

;;; 11.6 write an iterative version of union, called it-union

(defun it-union (l1 l2)
  (let ((result l1))
    (dolist (el l2 result)
      (unless (member el result)
	(push el result)))))

(assert (equal (it-union '(1 2 3) '(2 4 5)) '(5 4 1 2 3)))

;;; 11.8 write an iterative version of reverse, called it-reverse

(defun it-reverse (ls)
  (let ((result nil))
    (dolist (el ls result)
      (push el result))))

(assert (equal (it-reverse '(1 2 3)) '(3 2 1)))

;;; 11.9 show how to write check-all-odd using do

(defun check-all-odd (ls)
  (do ((el ls (cdr el)))
      ((null el) t)
    (when (evenp (first el))
      (return nil))))

(assert (check-all-odd '(1 3 5)))
(assert (not (check-all-odd '(1 3 4 5))))

;;; 11.10 show how to write launch using dotimes

;; example from book
(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))

(defun launch (n)
  (let ((cnt n))
    (dotimes (index n)
      (format t "~S..." cnt)
      (decf cnt)))
  (format t "Blast off!"))

;;; 11.11 rewrite the following function to use DO* instead of DOLIST

(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers)
		     largest)
      (when (> element largest)
	(setf largest element)))))

(defun find-largest (ls)
  (do* ((el ls (cdr el))
	(n (first el) (first el))
	(biggest (first el)))
       ((null el) biggest)
       (when (> n biggest)
	 (setf biggest n))))

(assert (= (find-largest '(1 2 3)) 3))
(assert (= (find-largest '(3 2 1)) 3))
(assert (not (find-largest nil)))

;;; 11.12 rewrite the following function to use DO instead of DOTIMES

;; from the book
(defun power-of-2 (n) ; 2 to the nth power
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))

(defun power-of-2 (n)
  (do ((result 1 (+ result result))
       (i 0 (+ i 1)))
      ((= i n) result)))

(assert (= (power-of-2 6) 64))
(assert (= (power-of-2 13) 8192))

;;; 11.13 rewrite the following function using dolist instead of do*

(defun first-non-integer (x)
  "Return the first non-integer element of x"
  (do* ((z x (rest z))
	(z1 (first z) (first z)))
       ((null z) 'none)
    (unless (integerp z1)
      (return z1))))

(defun first-non-integer (x)
  (dolist (el x 'none)
    (unless (integerp el)
      (return el))))

(assert (equal (first-non-integer '(1 2 a)) 'a))
(assert (equal (first-non-integer '(1 2 3)) 'none))

;;; 11.15 what is the bug in the following?

(defun ffo-with-do (x)
  (do ((z x (rest z))
       (e (first x) (first z)))
      ((null z) nil)
    (if (oddp e) (return e))))

(ffo-with-do '(2 4 6 7 8))
(ffo-with-do '(2 4 6 7)) ; last isn't processed because (first z) is using the previous iteration's value

;; 11.22 exercises assume the use of iterative solutions

;;; 11.22 a write a function complement-base that takes a base as input and returns the complementary base

(defvar bases
  '((A . T)
    (T . A)
    (G . C)
    (C . G)))

(defun complement-base (b)
  (cdr (assoc b bases)))

(assert (equal (complement-base 'A) 'T))
(assert (equal (complement-base 'T) 'A))

;;; 11.22 b write a function complement-strand that returns the complementary strand of dna

(defun complement-strand (ls)
  (do ((b ls (cdr b))
       (result nil (cons (complement-base (first b)) result)))
      ((null b) (reverse result))))

(assert (equal (complement-strand '(A G G T)) '(T C C A)))

;;; 11.22 c write a function make-double that takes a single strand as input and returns the double strand version

(defun make-double (ls)
  (do ((b ls (cdr b))
       (result '() (cons (list (first b) (complement-base (first b))) result)))
      ((null b) (reverse result))))

(assert (equal (make-double '(G G A C T)) '((G C) (G C) (A T) (C G) (T A))))

;;; 11.22 d write count-bases which counts the bases in a double or single strand

(defun flatten (ls)
  "flattens list one level"
  (flatten-1 ls))

(defun flatten-1 (ls &optional (acc '()))
  (cond
    ((null ls) (reverse acc))
    ((listp (first ls))
     (flatten-1 (rest ls)
		(append (first ls) acc)))
    (t
     (flatten-1 (rest ls)
		(cons (first ls) acc)))))

(assert (equal (flatten '((A) (B) C)) '(A B C)))

(defun count-bases (ls)
  (let ((fs (flatten ls)))
    (do ((el fs (cdr el))
	 (res '()))
	((null el) res)
      (if (null (assoc (first el) res))
	  (push (list (first el) 1) res)
	  (incf (second (assoc (first el) res)))))))
    
(assert (equal (count-bases '(A)) '((A 1))))
(assert (equal (count-bases '(A A)) '((A 2))))
(assert (equal (count-bases '((G C) (A T) (T A) (T A) (C G)))
	       '((A 3) (T 3) (G 2) (C 2))))
(assert (equal (count-bases '(A G T A C T C T))
	       '((C 2) (T 3) (G 1) (A 2))))

;;; 11.22 e write a predicate prefixp that returns t if one strand of dna is a prefix of another

(defun prefixp (prefix ls)
  (do* ((p prefix (cdr p))
	(l ls (cdr l)))
       ((or (null p)
	    (not (equal (first p) (first l))))
	(null p))))

(assert (prefixp '(a b c) '(a b c d)))
(assert (not (prefixp '(a b d) '(a b c d))))
(assert (prefixp '(g t c) '(g t c a t)))
(assert (not (prefixp '(g t c) '(a g t t c))))

;;; 11.22 f write a predicate appearsp that returns t if one strand appears inside another

(defun appearsp (target ls)
  (do* ((l ls (cdr l))
	(m (prefixp target l) (prefixp target l)))
       ((or (null l) m) m)))

(assert (appearsp '(c a t) '(t c a t g)))
(assert (not (appearsp '(c a t) '(t c c g t a))))

;;;; 11.22 g write a predicate coverp that returns t if its first input, repeated some number of times matches all of its second input

(defun repeat-list (n ls)
  "creates a list that is the list ls repeated n times"
  (do ((i 0 (+ i 1))
       (acc '() (append ls acc)))
      ((= i n) acc)))

(assert (equal (repeat-list 3 '(a b c)) '(a b c a b c a b c)))

(defun coverp (target ls)
  (multiple-value-bind (f r) (floor (length ls) (length target))
    (and (= r 0) (equal (repeat-list f target) ls))))

(assert (coverp '(a g c) '(a g c a g c a g c)))
(assert (not (coverp '(a g c) '(a g c t t g))))
(assert (not (coverp '(a g c) '(a g c t))))

;;; 11.22 h write a function prefix that returns the leftmost n bases of a dna strand

(defun prefix (n ls)
  (do ((i 0 (incf i))
       (el ls (cdr el))
       (res '()))
      ((or (null el)
	   (>= i n))
       (reverse res))
    (push (first el) res)))

(assert (equal (prefix 4 '(c g a t t a g)) '(c g a t)))
(assert (equal (prefix 4 '(c g)) '(c g)))
(assert (equal (prefix 0 '(c g a t t a g)) '()))

;;; 11.22 i write kernel, that finds the shortest prefix that can cover a strand

(defun kernel (ls)
  (do* ((strand (reverse ls)) ; enables us to cons the prefix
	(el ls (cdr el)) ; next element to attach
	(p (list (first ls)) (cons (first el) p)))
       ((coverp p strand) (reverse p))))

(assert (equal (kernel '(A G C A G C A G C)) '(A G C)))
(assert (equal (kernel '(A A A A A)) '(A)))
(assert (equal (kernel '(A G G T C)) '(A G G T C)))

;;; 11.22 j write a function write-dna that takes a single-stranded dna sequence as input and draws it along with its complementary strand

(defun format-seq (ls)
  "formats a sequence of elements with padding between them"
  (format nil "~{~3:@<~A~>~}" ls))

(defun write-dna (strand)
  (let ((comp (complement-strand strand)))
    (dolist (item 
	     (append
	      (format-strand strand)
	      (reverse (format-strand comp))))
      (format t "~&~A" item))))

(defun format-strand (strand)
  (let ((width (length strand)))
    (list
     (format-seq (make-list width :initial-element "---"))
     (format-seq (make-list width :initial-element "!"))
     (format-seq strand)
     (format-seq (make-list width :initial-element ".")))))

(write-dna '(A G G T C A T T G))
