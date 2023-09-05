;;; 6.1 why does the following equal nil?

(nth 4 '(A B C)) ; ran out of elements and the last element is nil

;;; 6.2 what is the value of the following and why.

(nth 3 '(A B C . D)) ; D not of type list, because nth would try to car

;;; 6.3 what is the value of

(last '(rosebud)) ; (rosebud)

;;; 6.4 what is the value of the following and why.

(last '((A B C))) ; ((A B C)) as the only element at top level list is a list

;;; 6.5 based on the following variable, write down what each of these expressions evalutes to

(defvar line '(roses are red))

(reverse line) ; (red are roses)

(first (last line)) ; red

(nth 1 line) ; are

(reverse (reverse line)) ; (roses are red)

(append line (list (first line))) ; (roses are red)

(append (last line) line) ; (red roses are red)

(list (first line) (last line)) ; (roses (red))

(cons (last line) line) ; ((red) roses are red)

(remove 'are line) ; (roses red)

(append line '(violets are blue)) ; (roses are red violets are blue)

;;; 6.6 (1) use last function to write a function called last-element that returns the last element of a list instead of the last cons cell.

(defun last-element (ls)
  (car (last ls)))

(assert (equal (last-element '(1 2 3)) 3))
(assert (equal (last-element '(1)) 1))

;;; 6.6 (2) write another version of last-element using reverse instead of last.

(defun last-element-reverse (ls)
  (car (reverse ls)))

(assert (equal (last-element-reverse '(1 2 3)) 3))
(assert (equal (last-element-reverse '(1)) 1))

;;; 6.6 (3) write another version using nth and length

(defun last-element-nth (ls)
  (nth (- (length ls) 1) ls))

(assert (equal (last-element-nth '(1 2 3)) 3))
(assert (equal (last-element-nth '(1)) 1))

;;; 6.7 (1) use reverse to write a next-to-last function that returns the next-to-last element of a list.

(defun next-to-last (ls)
  (cadr (reverse ls)))

(assert (equal (next-to-last '(1 2 3 4 5)) 4))
(assert (equal (next-to-last '(1 2)) 1))
(assert (equal (next-to-last '(1)) nil))

;;; 6.7 (2) write another version using nth

(defun next-to-last-nth (ls)
  (and (rest ls) ; ensure there is at least two elements
       (nth (- (length ls) 2) ls)))

(assert (equal (next-to-last-nth '(1 2 3 4 5)) 4))
(assert (equal (next-to-last-nth '(1 2)) 1))
(assert (equal (next-to-last-nth '(1)) nil))

;;; 6.8 write a function my-butlast that returns a list with the last element removed

(defun my-butlast (ls)
  (reverse (rest (reverse ls))))

(assert (equal (my-butlast '(roses are red)) '(roses are)))
(assert (equal (my-butlast '(G A G A)) '(G A G)))

;;; 6.9 what primitive function does the following reduce to (what is it the equivalent of)?

(defun mystery (x) (first (last (reverse x)))) ; first

;;; 6.10 a palindrome is a sequence that reads the same forwards and backwards. Write a function palindromep that returns t if its input is a palindrome.

(defun palindromep (ls)
  (equal ls (reverse ls)))

(assert (palindromep '(A B C D C B A)))
(assert (not (palindromep '(A B C A B C))))

;;; 6.11 write a function make-palindrome that makes a palindrome out of a list

(defun make-palindrome (ls)
  (append ls (reverse ls)))

(assert (equal (make-palindrome '(you and me)) '(you and me me and you)))
(assert (equal (make-palindrome '(1)) '(1 1)))

;;; 6.13 what is the result of intersecting a set with nil?

(intersection '(1 2 3) nil) ; nil

;;; 6.14 what is the result of intersecting a set with itself?

(defvar sa '(1 2 3))
(intersection sa sa) ; (3 2 1)

;;; 6.15 we can use member to write a predicate that returns a true value if a sentence contains the word "the"

(defun contains-the-p (sent)
  (member 'the sent))

;; suppose we want a predicate contains-article-p that returns a value if a sentence contains "the", "a", "an", write a version using intersection.

(defun contains-article-p (sent)
  (intersection sent '(the a an)))

(assert (contains-article-p '(test the tester)))
(assert (contains-article-p '(test a tester)))
(assert (not (contains-article-p '(test tester))))

;; write a version using member and or

(defun contains-article-mem-p (sent)
  (or (member 'the sent)
      (member 'a sent)
      (member 'an sent)))

(assert (contains-article-mem-p '(test the tester)))
(assert (contains-article-mem-p '(test a tester)))
(assert (not (contains-article-mem-p '(test tester))))

;; can this be solved with and instead of or?

(defun contains-article-and-p (sent)
  (not (and (not (member 'the sent))
	    (not (member 'a sent))
	    (not (member 'an sent)))))

(assert (contains-article-and-p '(test the tester)))
(assert (contains-article-and-p '(test a tester)))
(assert (not (contains-article-and-p '(test tester))))

;;; 6.16 what is the union of a set with nil?

(union '(1 2 3) nil) ; (1 2 3)

;;; 6.17 with is the union of a set with itself?

(defvar ua '(1 2 3))
(union ua ua) ; (1 2 3)

;;; 6.18 write a function add-vowels that takes a set of letters as input andds the vowels '(a e i o u) to the set.

(defun add-vowels (ls)
  (union ls '(A E I O U)))

(assert (equal (sort (add-vowels '(X A E Z)) #'string<) (sort '(X A E Z I O U) #'string<)))
(assert (equal (sort (add-vowels '()) #'string<) '(A E I O U)))

;;; 6.19 what are the results of using nil as an input to set-difference?

(set-difference '(1 2 3) nil) ; (1 2 3)
(set-difference nil '(1 2 3)) ; nil

;;; 6.21 if set x is a subset of set y, then subtracting y from x should leave the empty set. Write my-subsetp a version of subsetp that returns t if its first input is a subset of its second input.

(defun my-subsetp (a b)
  (not (set-difference a b)))

(assert (my-subsetp '(1 2 3) '(1 2 3 4)))
(assert (not (my-subsetp '(1 2 3) '(1 2 4))))

;;; 6.22 suppose the following variable, what will be the result of each of the following expressions

(defvar seta '(soap water))

(union seta '(no soap radio)) ; (no soap water radio)

(intersection seta (reverse seta)) ; (soap water)

(set-difference seta '(stop for water)) ; (soap)

(set-difference seta seta) ; nil

(member 'soap seta) ; (soap water)

(member 'water seta) ; (water)

(member 'washcloth seta) ; nil

;;; 6.24 sets are said to be equal if they contain exactly the same elements, even if in a different order. Write a set-equal predicate that returns t if two things are equal as sets.

(defun set-equal (a b)
  (and (subsetp a b)
       (subsetp b a)))

(assert (set-equal '(red blue green) '(green blue red)))
(assert (not (set-equal '(red blue green) '(red blue yellow))))

;;; 6.25 a set x is a proper subset of set y if x is a subset of y but not equal to y. Write the proper-subsetp predicate, which returns t if its first input is a proper subset of its second input.

(defun proper-subsetp (a b)
  (and (subsetp a b)
       (not (subsetp b a))))

(assert (proper-subsetp '(A C) '(A B C)))
(assert (not (proper-subsetp '(C A B) '(A B C))))

;;; 6.26 set of functions that compare descriptions of two objects and tell how many features they have in common
;;; i.e. (large red shiny cube -vs- small shiny red four-sided pyramid) should respond with (2 common features)

;;; 6.26 a write a function right-side that returns all the features to the right of the -vs- symbol

(defvar object-example '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defun right-side (ls)
  (rest (member '-vs- ls)))

(assert (equal (right-side object-example) '(small shiny red four-sided pyramid)))

;;; 6.26 b write a function left-side that returns all the features to the left of the -vs-

(defun left-side (ls)
  (reverse (right-side (reverse ls))))

(assert (equal (left-side object-example) '(large red shiny cube)))

;;; 6.26 c write a function count-common that returns the number of features the left and right sides have in common

(defun count-common (left right)
  (length (intersection left right)))

(assert (= (count-common (left-side object-example)
			 (right-side object-example))
	   2))

;;; 6.26 d write the main function, compare, that takes a list of features describing two objects, with a -vs- between them, and reports the number of features they have in common

(defun compare (ls)
  (list
   (count-common (left-side ls)
		 (right-side ls))
   'common
   'features))

(assert (equal (compare object-example)
	       '(2 common features)))
(assert (equal (compare '(small red metal cube -vs- red plastic small cube))
	       '(3 common features))) ; 6.26 e
