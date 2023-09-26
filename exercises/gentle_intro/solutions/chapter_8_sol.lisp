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

;;; 8.10 for x as a non-negative integer, and y a positive integer, x+y equals x+1 + (y-1). If y is zero, then x+y = x.
;;; use these equations to build a recursive version of + called rec-plus out of add1, sub1, cond, zerop. add1 and sub1 don't yet exist

(defun add1 (x)
  (+ x 1))

(defun sub1 (x)
  (- x 1))

(defun rec-plus (x y)
  (cond
    ((zerop y) x)
    (t (rec-plus (add1 x) (sub1 y)))))

(assert (= (rec-plus 1 2) 3))
(assert (= (rec-plus 0 0) 0))
(assert (= (rec-plus 10 1) 11))

;;; 8.11 fix the below fibonnaci to include the rules fib(0) = 1 and fib(1) = 1

;; example from book
(defun busted-fib (n)
  (+ (fib (- n 1))
     (fib (- n 2))))

(defun fib (n)
  (if (<= n 1) 1
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(assert (= (fib 4) 5))
(assert (= (fib 5) 8))

;;; 8.12 consider the following version of any-7-p, a recursive function that searches for the number seven in a list
;; provide an input that will cause infinite recursion
;; provide an input where it will work correctly

(defun any-7-p (ls)
  (cond ((equal (first ls) 7) t)
	(t (any-7-p (rest ls)))))

(any-7-p '(1 2 3)) ; infinite recursion
(any-7-p '(1 2 7)) ; success

;;; 8.17 use double-test tail recursion to write find-first-odd, a function that returns the first odd number in a list, or nil if there are none

;; example template of double-test tail recursion
(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))

(defun find-first-odd (ls)
  (cond ((null ls) nil)
	((oddp (first ls)) (first ls))
	(t (find-first-odd (rest ls)))))

(assert (equal (find-first-odd '(2 3 7)) 3))
(assert (equal (find-first-odd '(2 4)) nil))
(assert (equal (find-first-odd '()) nil))

;;; 8.18 use single-test tail recursion to write last-element, a function that returns the last element of a list

(defun last-element (ls)
  (if (atom (cdr ls)) (car ls) (last-element (cdr ls))))

(assert (equal (last-element '(1 2 3)) 3))
(assert (equal (last-element '()) nil))

;;; 8.21 write a recursive function add-nums that adds up the numbers n, n-1, n-2 and so on, down to 0, and returns the result

(defun add-nums (n)
  (if (<= n 0) 0 (+ n (add-nums (- n 1)))))

(assert (= (add-nums 3) 6))
(assert (= (add-nums 0) 0))
(assert (= (add-nums -1) 0))

;;; 8.22 write a recursive function all-equal that returns t if the first element of a list is equal to the second, second to third, and so on

(defun all-equal (ls)
  (cond
    ((null (rest ls)) t) ; single element left
    ((not (equal (first ls) (second ls))) nil) ; two elements, compare them
    (t (all-equal (rest ls)))))

(assert (all-equal '(i i i i)))
(assert (not (all-equal '(i i e i))))
(assert (all-equal '(i)))
(assert (all-equal '()))

;;; 8.24 write count-down, a function that counts down from n using list-consing recursion

(defun count-down (n)
  (cond
    ((<= n 0) '())
    (t (cons n (count-down (- n 1))))))

(assert (equal (count-down 5) '(5 4 3 2 1)))
(assert (equal (count-down 0) '()))
(assert (equal (count-down -1) '()))

;;; 8.25 using count-down, write an applicative version of fact (factorial)

(defun my-fact (n)
  (reduce #'* (count-down n)))

(assert (= (my-fact 3) 6))

;;; 8.26 adjust count-down so that it counts down to 0

(defun count-down (n)
  (cond
    ((<= n 0) '(0))
    (t (cons n (count-down (- n 1))))))

(assert (equal (count-down 5) '(5 4 3 2 1 0)))
(assert (equal (count-down 0) '(0)))
(assert (equal (count-down -1) '(0)))

;;; 8.27 write square-list, a recursive function that takes a list of numbers as input and returns a list of their squares

(defun square-list (ls)
  (if (null ls) '()
      (cons (* (first ls) (first ls))
	    (square-list (rest ls)))))

(assert (equal (square-list '(3 4 5 6)) '(9 16 25 36)))
(assert (equal (square-list '()) '()))

;;; 8.28 modify the below so that recursion stops as soon as the function runs off the end of the list

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	((null x) nil)
	(t (my-nth (- n 1) (rest x)))))

(assert (equal (my-nth 2 '(a b c)) 'c))
(assert (equal (my-nth 2 '(a b)) nil))
(assert (equal (my-nth 2 '()) nil))
(assert (equal (my-nth 1000000 '()) nil))

;;; 8.29 write my-member, a recursive version of member

(defun my-member (item list)
  (cond
    ((null list) nil)
    ((equal item (first list)) list)
    (t (my-member item (rest list)))))

(assert (equal (my-member 'b '(a b c)) '(b c)))
(assert (equal (my-member 'd '(a b c)) nil))

;;; 8.30 write my-assoc, a recursive assoc

(defun my-assoc (item alist)
  (cond
    ((null alist) nil)
    ((equal item (first (first alist))) (first alist))
    (t (my-assoc item (rest alist)))))

(assert (equal (my-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (equal (my-assoc 'd '((a 1) (b 2) (c 3))) nil))

;;; 8.31 write a function compare-lengths that determines which list is longer (don't use length)

(defun compare-lengths (l1 l2)
  (cond
    ((and (null l1) (null l2)) 'same-length)
    ((null l1) 'second-is-longer)
    ((null l2) 'first-is-longer)
    (t (compare-lengths (rest l1) (rest l2)))))

(assert (equal (compare-lengths '(a) '(a b)) 'second-is-longer))
(assert (equal (compare-lengths '(a b) '(a)) 'first-is-longer))
(assert (equal (compare-lengths '(a) '(a)) 'same-length))

;;; 8.32 write a function sum-numeric-elements, which adds up all the numbers in a list, ignoring non-numbers

(defun sum-numeric-elements (ls)
  (cond
    ((null ls) 0)
    ((numberp (first ls)) (+ (first ls) (sum-numeric-elements (rest ls))))
    (t (sum-numeric-elements (rest ls)))))

(assert (= (sum-numeric-elements '(3 bears 3 bowls and 1 girl)) 7))
(assert (= (sum-numeric-elements '(2 girls 1 cup)) 3))
(assert (= (sum-numeric-elements '(no numbers)) 0))

;;; 8.33 write my-remove, a recursive version of the remove function

(defun my-remove (item list)
  (cond
    ((null list) '())
    ((equal item (first list)) (my-remove item (rest list)))
    (t (cons (first list) (my-remove item (rest list))))))

(assert (equal (my-remove 'a '(a b c a)) '(b c)))
(assert (equal (my-remove 'a '()) '()))

;;; 8.34 write my-intersection, a recursive version of the intersection function

(defun my-intersection (l1 l2)
  (cond
    ((null l1) '())
    ((member (first l1) l2) (cons (first l1) (my-intersection (rest l1) l2)))
    (t (my-intersection (rest l1) l2))))

(assert (equal (my-intersection '(a b c) '(b)) '(b)))
(assert (equal (my-intersection '(a b c) '(d e f)) '()))
(assert (equal (my-intersection '(a b c) '()) '()))

;;; 8.35 write my-set-difference, a recursive version of the set-difference

(defun my-set-difference (l1 l2)
  (cond
    ((null l1) l2)
    ((member (first l1) l2)
     (my-set-difference
      (remove (first l1) l1)
      (remove (first l1) l2)))
    (t (cons (first l1) (my-set-difference (rest l1) l2)))))

(assert (equal (my-set-difference '(a b c d) '(b d)) '(a c)))
(assert (equal (my-set-difference '(a b c d) '()) '(a b c d)))
(assert (equal (my-set-difference '() '(a b c d)) '(a b c d)))

;;; 8.36 count-odd counts the odd number of odd elements in a list of numbers
;; implement both using conditional augmentation and regular augmenting recursion

(defun count-odd (ls)
  (cond
    ((null ls) 0)
    ((oddp (first ls)) (+ 1 (count-odd-cond (rest ls))))
    (t (count-odd-cond (rest ls)))))

(defun count-odd (ls)
  (cond
    ((null ls) 0)
    (t (+ (if (oddp (first ls)) 1 0) (count-odd-cond (rest ls))))))

(assert (= (count-odd '(1 3 4 5)) 3))
(assert (= (count-odd '()) 0))
(assert (= (count-odd '(2 4 6)) 0))

;;; 8.39 write a function count-atoms that returns the numbers of atoms in a tree

(defun count-atoms (ls)
  (cond
    ((null ls) 1)
    ((atom ls) 1)
    (t (+ (count-atoms (car ls))
	  (count-atoms (cdr ls))))))

(assert (= (count-atoms '(a (b) c)) 5))
(assert (= (count-atoms '()) 1))

;;; 8.40 write count-cons a function that returns the number of cons cells in a tree

(defun count-cons (ls)
  (cond
    ((null ls) 0)
    ((consp ls) (+ 1 (count-cons (car ls) (cdr ls))))
    (t 0)))

(assert (= (count-cons '(asdf)) 1))
(assert (= (count-cons '((asdf))) 2))

;;; 8.41 write a function sum-tree that returns the sum of all the numbers appearing in a tree

(defun sum-tree (ls)
  (cond
    ((null ls) 0)
    ((numberp ls) ls)
    ((consp ls) (+ (sum-tree (car ls)) (sum-tree (cdr ls))))
    (t 0)))

(assert (= (sum-tree '((3 bears) (3 bowls) (1 girl))) 7))
(assert (= (sum-tree '(() () ())) 0))
(assert (= (sum-tree '()) 0))

;;; 8.42 write my-subst, a recursive version of the subst function

(defun my-subst (new old tree)
  (cond
    ((null tree) nil)
    ((equal tree old) new)
    ((consp tree) (cons (my-subst new old (car tree))
			(my-subst new old (cdr tree))))
    (t tree)))

(assert (equal (my-subst 1 'a '(a b c)) '(1 b c)))
(assert (equal (my-subst 1 'a '((a b) (c (d a) e))) '((1 b) (c (d 1) e))))

;;; 8.43 write flatten, a function that returns all the elements of an arbitrarily nested list in a single-level list

(defun flatten (tree)
  (cond
    ((null tree) nil)
    ((consp tree) (append (flatten (car tree))
			  (flatten (cdr tree))))
    (t (list tree))))

(assert (equal (flatten '(a (b) c)) '(a b c)))
(assert (equal (flatten '((a b (r)) a c (a d ((a (b)) r) a))) '(a b r a c a d a b r a)))

;;; 8.44 write a function tree-pdeth that returns the maximum depth of a binary tree

(defun tree-depth (tree)
  (cond
    ((consp tree)
     (+ 1 (max
	   (tree-depth (car tree))
	   (tree-depth (cdr tree)))))
    (t 0)))

(assert (= (tree-depth '(a . b)) 1))
(assert (= (tree-depth '((a . b) . (c . d))) 2))

;;; 8.45 write a function paren-depth that returns the maximum depth of nested parens

(defun paren-depth (tree)
  (cond
    ((atom tree) 0)
    (t (max (+ 1 (paren-depth (car tree)))
	    (paren-depth (cdr tree))))))

(assert (= (paren-depth '(a b c)) 1))
(assert (= (paren-depth '(a b ((c) d) e)) 3))

;;; 8.46 implement count-up where you recursively add an element to the end of the list

(defun count-up (n)
  (cond
    ((= n 0) nil)
    (t (append (count-up (- n 1)) (list n)))))

(assert (equal (count-up 5) '(1 2 3 4 5)))
(assert (equal (count-up 0) nil))

;;; 8.47 write make-loaf, a function that returns a loaf of size n using if

(defun make-loaf (n)
  (if (<= n 0) nil
      (cons 'X (make-loaf (- n 1)))))

(assert (equal (make-loaf 4) '(X X X X)))

;;; 8.48 write a recursive bury function that buries an item under n levels of parens

(defun bury (item depth)
  (cond
    ((<= depth 0) item)
    (t (list (bury item (- depth 1))))))

(assert (equal (bury 'fred 2) '((fred))))

;;; 8.49 write pairings, a function that pairs the elements of two lists (assume equal length)

(defun pairings (l1 l2)
  (cond
    ((or (null l1) (null l2)) nil)
    (t (cons (list (first l1) (first l2))
	     (pairings (rest l1) (rest l2))))))

(assert (equal (pairings '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3))))

;;; 8.50 write sublists, a funciton that returns the successive sublists of a list

(defun sublists (ls)
  (cond
    ((null ls) nil)
    (t (cons ls
	     (sublists (rest ls))))))

(assert (equal (sublists '(fee fie foe)) '((fee fie foe) (fie foe) (foe))))

;;; 8.51 write my-reverse using a helper function and a recursive function of two inputs

(defun my-reverse (ls)
  (my-reverse-2 ls '()))

(defun my-reverse-2 (ls acc)
  (if (null ls) acc
      (my-reverse-2 (rest ls) (cons (first ls) acc))))

(assert (equal (my-reverse '(a b c)) '(c b a)))
(assert (equal (my-reverse '()) '()))

;;; 8.52 write my-union a recursive version of union

(defun my-union (l1 l2)
  (cond
    ((null l1) l2)
    ((member (first l1) l2)
     (my-union (rest l1) l2))
    (t
     (cons (first l1) (my-union (rest l1) l2)))))

(assert (equal (my-union '(a b c) '(a c d)) '(b a c d)))
(assert (equal (my-union '(a b) '(c)) '(a b c)))

;;; 8.53 write largest-even, a recursive function that returns the largest even number in a list of non-negative integers
;;; use max function to return largest of two inputs

(defun largest-even (ls)
  (cond
    ((null ls) 0)
    ((oddp (first ls)) (largest-even (rest ls)))
    (t (max (first ls) (largest-even (rest ls))))))

(assert (= (largest-even '(5 2 4 3)) 4))
(assert (= (largest-even nil) 0))

;;; 8.54 write a recursive function huge that raises a number to its own power
;;; dont use reduce

(defun huge (n)
  (huge-2 n n))

(defun huge-2 (n c)
  (cond
    ((= c 0) 1)
    (t (* n (huge-2 n (- c 1))))))

(assert (= (huge 2) 4))
(assert (= (huge 3) 27))
(assert (= (huge 4) 256))

;;; 8.56 write every-other, a recursive function that returns every other element of a list

(defun every-other (ls)
  (if (null ls) '()
      (cons (first ls)
	    (every-other (cddr ls)))))

(assert (equal (every-other '(a b c d e f g)) '(a c e g)))
(assert (equal (every-other '(i came i saw i conquered)) '(i i i)))

;;; 8.57 write left-half, a recursive function in two parts that returns the first n/2 elements of a list length n, write your function so that the list does not have to be of even length
;;; you can use length but not reverse

(defun left-half (ls)
  (take (/ (length ls) 2) ls))

(defun take (n ls)
  (if (<= n 0) '() (cons (first ls) (take (- n 1) (rest ls)))))

(assert (equal (left-half '(a b c d e)) '(a b c)))
(assert (equal (left-half '(1 2 3 4 5 6 7 8)) '(1 2 3 4)))

;;; 8.58 merge-lists, a function that takes two lists (in ascending order), and merges them in order

(defun merge-lists (l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    ((< (first l1) (first l2))
     (cons (first l1)
	   (merge-lists (rest l1) l2)))
    (t
     (cons (first l2)
	   (merge-lists l1 (rest l2))))))

(assert (equal (merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13)) '(1 2 2 3 5 6 8 9 10 12 13)))

;;; 8.60

; (name father mother)
(defvar family
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deidre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))

;;; 8.60 a write functions father, mother, parents, children that return a person's farther, mother, al ist of his or her known parents, and al ist of his or her children

(defun father (name)
  (second (assoc name family)))

(defun mother (name)
  (third (assoc name family)))

(defun parents (name)
  (remove-if #'null (rest (assoc name family))))

(defun children (name)
  (and name
       (mapcar #'first
	       (remove-if-not
		#'(lambda (r)
		    (or (equal (second r) name)
			(equal (third r) name)))
		family))))
  
(assert (equal (father 'suzanne) 'colin))
(assert (equal (parents 'suzanne) '(colin deidre)))
(assert (equal (parents 'frederick) '(tamara))) ; no dad
(assert (equal (children 'arthur) '(bruce charles david ellen)))
(assert (equal (father nil) nil))
(assert (equal (mother nil) nil))
(assert (equal (parents nil) nil))
(assert (equal (children nil) nil))

;;; 8.60 b write siblings, a function that returns al ist of a person's siblings, including half-siblings

(defun siblings (name)
  ;; based on parents, get list of all children
  (remove-if #'(lambda (sib) (equal sib name))
	     (reduce #'union ; caters for both parents having a kid together, and half siblings
		    (mapcar #'(lambda (p) (children p)) (parents name)))))

(assert (equal (siblings 'bruce) '(charles david ellen)))
(assert (equal (siblings 'zelda) '(joshua)))

;;; 8.60 c write mapunion, an applicative operator that takes a function and al ist as input, and applies the function to every element of the list, and computes the union of all the results

(defun mapunion (f ls)
  (and ls
       (reduce #'union
	       (mapcar f ls))))

(assert (equal (mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d)))
	       '(j e f a b c d)))

;;; 8.60 d write grandparents, a function that returns the set of a persons grandparents, use mapunion

(defun grandparents (name)
  (mapunion #'parents
	    (parents name)))

(assert (equal (grandparents 'tamara) '(kate arthur colin deidre)))

;;; 8.60 e write cousins (children of any of their parents siblings), use mapunion

(defun cousins (name)
  (mapunion #'children
	    (mapunion #'siblings
		      (parents name))))

(assert (equal (cousins 'julie) '(vincent tamara nigel)))

;;; 8.60 f write the two-input recursive predicate descended-from that returns a true value if the first person is descended from the second

(defun descended-from (descendent ancestor)
  (let ((cls (children ancestor)))
    (cond
      ((null cls) nil)
      ((member descendent cls) t)
      (t (remove-if-not #'identity
		 (mapcar #'(lambda (c) (descended-from descendent c))
			 cls))))))
				    

(assert (descended-from 'tamara 'arthur))
(assert (not (descended-from 'tamara 'linda)))

;;; 8.60 g write the recursive function ancestors that returns a person's set of ancestors

(defun ancestors (name)
  (let ((ps (parents name)))
    (append ps (mapunion #'ancestors ps))))

(assert (equal (ancestors 'marie) '(george ellen linda frank arthur kate)))

;;; 8.60 h write a recursive function generation-gap that returns the number of generations seperating a person and one of his or her ancestors

(defun generation-gap (descendent ancestor &optional (counter 1))
  (let ((ps (parents descendent)))
    (cond
      ((null ps) nil)
      ((member ancestor ps) counter)
      (t (find-if #'identity (mapcar #'(lambda (p) (generation-gap p ancestor (+ counter 1))) ps))))))

(assert (= (generation-gap 'suzanne 'colin) 1))
(assert (= (generation-gap 'frederick 'colin) 3))
(assert (not (generation-gap 'frederick 'linda)))

;;; 8.60 i

;; is robert descended from deidre

(descended-from 'robert 'deidre) ; no

;; who are yvette's ancestors

(ancestors 'yvette) ; (ROBERT ZELDA FRANK LINDA ELLEN GEORGE JULIE QUENTIN VINCENT WANDA BRUCE SUZANNE KATE ARTHUR COLIN DEIDRE)

;; what is the generation gap between olivia and frank

(generation-gap 'olivia 'frank) ; 3

;; who are peter's cousins

(cousins 'peter) ; (joshua robert)

;; who are olivias grandparents

(grandparents 'olivia) ; (hillary andre george ellen)

;;; 8.61 write a tail-recursive version of count-up

(defun count-up (n)
  (count-up-2 n))

(defun count-up-2 (n &optional (acc '()))
  (if (= n 0) acc
      (count-up-2 (- n 1)
		  (cons n acc))))

(assert (equal (count-up 5) '(1 2 3 4 5)))
(assert (equal (count-up 0) nil))

;;; 8.62 write a tail-recursive version of fact

(defun my-fact (n)
  (my-fact-2 (count-up n)))

(defun my-fact-2 (ls &optional (acc 1))
  (if (null ls) acc
      (my-fact-2 (rest ls)
		 (* (first ls) acc))))

(assert (= (my-fact 3) 6))

;;; 8.63 write tail-recursive versions of union, intersection, and set-difference

(defun my-union (l1 l2)
  (my-union-2 (append l1 l2)))

(defun my-union-2 (ls &optional (acc '()))
  (cond
    ((null ls) acc)
    ((member (first ls) acc) (my-union-2 (rest ls) acc))
    (t (my-union-2 (rest ls) (cons (first ls) acc)))))

(assert (equal (my-union '(a b c) '(a c d)) '(d c b a)))
(assert (equal (my-union '(a b) '(c)) '(c b a)))

(defun my-intersection (l1 l2)
  (my-intersection-2 l1 l2))

(defun my-intersection-2 (l1 l2 &optional (acc '()))
  (cond
    ((null l1) acc)
    ((member (first l1) l2)
     (my-intersection-2 (rest l1) l2 (cons (first l1) acc)))
    (t
     (my-intersection-2 (rest l1) l2 acc))))

(assert (equal (my-intersection '(a b c) '(b)) '(b)))
(assert (equal (my-intersection '(a b c) '(d e f)) '()))
(assert (equal (my-intersection '(a b c) '()) '()))

(defun my-set-difference (l1 l2)
  (my-set-difference-2 l1 l2))

(defun my-set-difference-2 (l1 l2 &optional (acc '()))
  (cond
    ((null l1) (append l2 acc))
    ((member (first l1) l2)
     (my-set-difference-2 (remove (first l1) l1)
			  (remove (first l1) l2)
			  acc))
    (t
     (my-set-difference-2 (remove (first l1) l1)
			  (remove (first l1) l2)
			  (cons (first l1) acc)))))

(assert (equal (my-set-difference '(a b c d) '(b d)) '(c a)))
(assert (equal (my-set-difference '(a b c d) '()) '(d c b a)))
(assert (equal (my-set-difference '() '(a b c d)) '(a b c d)))

;;; 8.64 write a tree-find-if operator that returns the first non-nil atom of a tree that satisfies a predicate

(defun tree-find-if (fn ls)
  (cond
    ((null ls) nil)
    ((listp (first ls)) (or (tree-find-if fn (first ls))
			   (tree-find-if fn (rest ls))))
    ((funcall fn (first ls)) (first ls))
    (t (tree-find-if fn (rest ls)))))

(assert (equal (tree-find-if #'oddp '((2 4) (5 6) 7)) 5))
(assert (equal (tree-find-if #'oddp '((2 4) (6 5) 7)) 5))

;;; 8.65 use labels to write versions of tr-count-slices and tr-reverse

(defun tr-count-slices (loaf)
  (labels ((count-slices (ls total)
	     (if (null ls) total
		 (count-slices (rest ls) (+ 1 total)))))
    (count-slices loaf 0)))

(assert (= (tr-count-slices '(x x x)) 3))

(defun tr-reverse (list)
  (labels ((rev (ls acc)
	     (if (null ls) acc
		 (rev (rest ls)
		      (cons (first ls) acc)))))
    (rev list '())))

(assert (equal (tr-reverse '(a b c)) '(c b a)))

;;; 8.66 write arith-eval, a funciton that evaluates arithmetic expressions

(defun arith-eval (ls &optional (acc 0))
  (cond
    ((null ls) acc)
    ((symbolp (first ls))
     (arith-eval (cddr ls)
		 (funcall (first ls) acc (arith-resolve (second ls)))))
    (t (arith-eval (rest ls)
		   (arith-resolve (first ls))))))

(defun arith-resolve (item)
  (cond
    ((listp item)
     (arith-eval item))
    ((numberp item) item)
    (t nil)))
     
(assert (= (arith-eval '(1 + 1)) 2))
(assert (= (arith-eval '((2 * 2)) 4)))
(assert (= (arith-eval '(2 + (3 * 4))) 14))

;;; 8.67 write a predicate legalp that returns t if its input is a legal arithmetic expression

(defun legalp (ls)
  ;; legal expressions are
  (cond
    ((null ls) nil)
    ;; number
    ((numberp ls) t)
    ;; a single nested item
    ((and (listp ls)
	  (= (length ls) 1)
	  (legalp (first ls))))
    ;; expression of three elements
    ((and (listp ls)
          (>= (length ls) 3)
	  (legalp (first ls))
	  (operatorp (second ls))
	  (legalp (third ls)))
     (let ((rem (cdddr ls)))
       (or (null rem)
	   (legalp rem))))
    (t nil)))
    
(defun operatorp (item)
  (member item '(+ - * /)))

(assert (legalp 4))
(assert (legalp '((2 * 2) - 3)))
(assert (not (legalp nil)))
(assert (not (legalp '(a b c d))))

;;; 8.69 write a function that can generate the factorisation tree for calculating primes
;;; nb slightly different than 69-70 in the book

(defvar divs '(2 3 5))

(defun primes (n)
  (let ((res
	  (find-if
	   #'(lambda (v)
	       (let ((r (nth-value 1 (floor n v))))
		 (= r 0)))
	   divs)))
    (if (and res (not (equal res n)))
	(cons res
	      (primes (floor n res)))
	(list n))))

(assert (equal (primes 60) '(2 2 3 5)))
