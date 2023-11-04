;;; 8.4 write a function called laugh that takes a number as input and returns al ist of that many has

;; a scaffold is provided
;; (defun laugh (n)
;;  (cond (A B)
;;	(t (cons 'ha C))))

(assert (equal (laugh 3) '(ha ha ha)))
(assert (equal (laugh 0) '()))
(assert (equal (laugh -1) '()))

;;; 8.5 write a recursive function add-up to add up all the numbers in a list

(assert (= (add-up '(2 3 7)) 12))
(assert (= (add-up '()) 0))

;;; 8.6 write alloddp, a recursive function that returns t if all the numbers in a list are odd

(assert (alloddp '(1 3 7)))
(assert (alloddp '()))
(assert (not (alloddp '(1 4 5))))

;;; 8.7 write rec-member, a recursive version of member

(assert (equal (rec-member 'b '(a b c)) '(b c)))
(assert (equal (rec-member 'd '(a b c)) nil))
(assert (equal (rec-member 'd '()) nil))

;;; 8.8 write rec-assoc, a recursive version of assoc

(assert (equal (rec-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (not (rec-assoc 'd '((a 1) (b 2) (c 3)))))
(assert (not (rec-assoc 'd '())))

;;; 8.9 write rec-nth, a recursive version of nth

(assert (equal (rec-nth 2 '(a b c)) 'c))
(assert (equal (rec-nth 2 '(a b)) nil))
(assert (equal (rec-nth 0 '(a b)) 'a))
(assert (equal (rec-nth -1 '(a b)) 'a))

;;; 8.10 for x as a non-negative integer, and y a positive integer, x+y equals x+1 + (y-1). If y is zero, then x+y = x.
;;; use these equations to build a recursive version of + called rec-plus out of add1, sub1, cond, zerop. add1 and sub1 don't yet exist

(assert (= (rec-plus 1 2) 3))
(assert (= (rec-plus 0 0) 0))
(assert (= (rec-plus 10 1) 11))

;;; 8.11 fix the below fibonnaci to include the rules fib(0) = 1 and fib(1) = 1

;; example from book
(defun busted-fib (n)
  (+ (fib (- n 1))
     (fib (- n 2))))

(assert (= (fib 4) 5))
(assert (= (fib 5) 8))

;;; 8.12 consider the following version of any-7-p, a recursive function that searches for the number seven in a list
;; provide an input that will cause infinite recursion
;; provide an input where it will work correctly

(defun any-7-p (ls)
  (cond ((equal (first ls) 7) t)
	(t (any-7-p (rest ls)))))

;;; 8.17 use double-test tail recursion to write find-first-odd, a function that returns the first odd number in a list, or nil if there are none

;; example template of double-test tail recursion
(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))

(assert (equal (find-first-odd '(2 3 7)) 3))
(assert (equal (find-first-odd '(2 4)) nil))
(assert (equal (find-first-odd '()) nil))

;;; 8.18 use single-test tail recursion to write last-element, a function that returns the last element of a list

(assert (equal (last-element '(1 2 3)) 3))
(assert (equal (last-element '()) nil))

;;; 8.21 write a recursive function add-nums that adds up the numbers n, n-1, n-2 and so on, down to 0, and returns the result

(assert (= (add-nums 3) 6))
(assert (= (add-nums 0) 0))
(assert (= (add-nums -1) 0))

;;; 8.22 write a recursive function all-equal that returns t if the first element of a list is equal to the second, second to third, and so on

(assert (all-equal '(i i i i)))
(assert (not (all-equal '(i i e i))))
(assert (all-equal '(i)))
(assert (all-equal '()))

;;; 8.24 write count-down, a function that counts down from n using list-consing recursion

(assert (equal (count-down 5) '(5 4 3 2 1)))
(assert (equal (count-down 0) '()))
(assert (equal (count-down -1) '()))

;;; 8.25 using count-down, write an applicative version of fact (factorial)

(assert (= (my-fact 3) 6))

;;; 8.26 adjust count-down so that it counts down to 0

(assert (equal (count-down 5) '(5 4 3 2 1 0)))
(assert (equal (count-down 0) '(0)))
(assert (equal (count-down -1) '(0)))

;;; 8.27 write square-list, a recursive function that takes a list of numbers as input and returns a list of their squares

(assert (equal (square-list '(3 4 5 6)) '(9 16 25 36)))
(assert (equal (square-list '()) '()))

;;; 8.28 modify the below so that recursion stops as soon as the function runs off the end of the list

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))

(assert (equal (my-nth 2 '(a b c)) 'c))
(assert (equal (my-nth 2 '(a b)) nil))
(assert (equal (my-nth 2 '()) nil))
(assert (equal (my-nth 1000000 '()) nil))

;;; 8.29 write my-member, a recursive version of member

(assert (equal (my-member 'b '(a b c)) '(b c)))
(assert (equal (my-member 'd '(a b c)) nil))

;;; 8.30 write my-assoc, a recursive assoc

(assert (equal (my-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (equal (my-assoc 'd '((a 1) (b 2) (c 3))) nil))

;;; 8.31 write a function compare-lengths that determines which list is longer (don't use length)

(assert (equal (compare-lengths '(a) '(a b)) 'second-is-longer))
(assert (equal (compare-lengths '(a b) '(a)) 'first-is-longer))
(assert (equal (compare-lengths '(a) '(a)) 'same-length))

;;; 8.32 write a function sum-numeric-elements, which adds up all the numbers in a list, ignoring non-numbers

(assert (= (sum-numeric-elements '(3 bears 3 bowls and 1 girl)) 7))
(assert (= (sum-numeric-elements '(2 girls 1 cup)) 3))
(assert (= (sum-numeric-elements '(no numbers)) 0))

;;; 8.33 write my-remove, a recursive version of the remove function

(assert (equal (my-remove 'a '(a b c a)) '(b c)))
(assert (equal (my-remove 'a '()) '()))

;;; 8.34 write my-intersection, a recursive version of the intersection function

(assert (equal (my-intersection '(a b c) '(b)) '(b)))
(assert (equal (my-intersection '(a b c) '(d e f)) '()))
(assert (equal (my-intersection '(a b c) '()) '()))

;;; 8.35 write my-set-difference, a recursive version of the set-difference

(assert (equal (my-set-difference '(a b c d) '(b d)) '(a c)))
(assert (equal (my-set-difference '(a b c d) '()) '(a b c d)))
(assert (equal (my-set-difference '() '(a b c d)) '(a b c d)))

;;; 8.36 count-odd counts the odd number of odd elements in a list of numbers
;; implement both using conditional augmentation and regular augmenting recursion

(assert (= (count-odd '(1 3 4 5)) 3))
(assert (= (count-odd '()) 0))
(assert (= (count-odd '(2 4 6)) 0))

;;; 8.39 write a function count-atoms that returns the numbers of atoms in a tree

(assert (= (count-atoms '(a (b) c)) 5))
(assert (= (count-atoms '()) 1))

;;; 8.40 write count-cons a function that returns the number of cons cells in a tree

(assert (= (count-cons '(asdf)) 1))
(assert (= (count-cons '((asdf))) 2))

;;; 8.41 write a function sum-tree that returns the sum of all the numbers appearing in a tree

(assert (= (sum-tree '((3 bears) (3 bowls) (1 girl))) 7))
(assert (= (sum-tree '(() () ())) 0))
(assert (= (sum-tree '()) 0))

;;; 8.42 write my-subst, a recursive version of the subst function

(assert (equal (my-subst 1 'a '(a b c)) '(1 b c)))
(assert (equal (my-subst 1 'a '((a b) (c (d a) e))) '((1 b) (c (d 1) e))))

;;; 8.43 write flatten, a function that returns all the elements of an arbitrarily nested list in a single-level list

(assert (equal (flatten '(a (b) c)) '(a b c)))
(assert (equal (flatten '((a b (r)) a c (a d ((a (b)) r) a))) '(a b r a c a d a b r a)))

;;; 8.44 write a function tree-pdeth that returns the maximum depth of a binary tree

(assert (= (tree-depth '(a . b)) 1))
(assert (= (tree-depth '((a . b) . (c . d))) 2))

;;; 8.45 write a function paren-depth that returns the maximum depth of nested parens

(assert (= (paren-depth '(a b c)) 1))
(assert (= (paren-depth '(a b ((c) d) e)) 3))

;;; 8.46 implement count-up where you recursively add an element to the end of the list

(assert (equal (count-up 5) '(1 2 3 4 5)))
(assert (equal (count-up 0) nil))

;;; 8.47 write make-loaf, a function that returns a loaf of size n using if

(assert (equal (make-loaf 4) '(X X X X)))

;;; 8.48 write a recursive bury function that buries an item under n levels of parens

(assert (equal (bury 'fred 2) '((fred))))

;;; 8.49 write pairings, a function that pairs the elements of two lists (assume equal length)

(assert (equal (pairings '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3))))

;;; 8.50 write sublists, a funciton that returns the successive sublists of a list

(assert (equal (sublists '(fee fie foe)) '((fee fie foe) (fie foe) (foe))))

;;; 8.51 write my-reverse using a helper function and a recursive function of two inputs

(assert (equal (my-reverse '(a b c)) '(c b a)))
(assert (equal (my-reverse '()) '()))

;;; 8.52 write my-union a recursive version of union

(assert (equal (my-union '(a b c) '(a c d)) '(b a c d)))
(assert (equal (my-union '(a b) '(c)) '(a b c)))

;;; 8.53 write largest-even, a recursive function that returns the largest even number in a list of non-negative integers
;;; use max function to return largest of two inputs

(assert (= (largest-even '(5 2 4 3)) 4))
(assert (= (largest-even nil) 0))

;;; 8.54 write a recursive function huge that raises a number to its own power
;;; dont use reduce

(assert (= (huge 2) 4))
(assert (= (huge 3) 27))
(assert (= (huge 4) 256))

;;; 8.56 write every-other, a recursive function that returns every other element of a list

(assert (equal (every-other '(a b c d e f g)) '(a c e g)))
(assert (equal (every-other '(i came i saw i conquered)) '(i i i)))

;;; 8.57 write left-half, a recursive function in two parts that returns the first n/2 elements of a list length n, write your function so that the list does not have to be of even length
;;; you can use length but not reverse

(assert (equal (left-half '(a b c d e)) '(a b c)))
(assert (equal (left-half '(1 2 3 4 5 6 7 8)) '(1 2 3 4)))

;;; 8.58 merge-lists, a function that takes two lists (in ascending order), and merges them in order

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

(assert (equal (father 'suzanne) 'colin))
(assert (equal (parents 'suzanne) '(colin deidre)))
(assert (equal (parents 'frederick) '(tamara))) ; no dad
(assert (equal (children 'arthur) '(bruce charles david ellen)))
(assert (equal (father nil) nil))
(assert (equal (mother nil) nil))
(assert (equal (parents nil) nil))
(assert (equal (children nil) nil))

;;; 8.60 b write siblings, a function that returns al ist of a person's siblings, including half-siblings

(assert (equal (siblings 'bruce) '(charles david ellen)))
(assert (equal (siblings 'zelda) '(joshua)))

;;; 8.60 c write mapunion, an applicative operator that takes a function and al ist as input, and applies the function to every element of the list, and computes the union of all the results

(assert (equal (mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d)))
	       '(j e f a b c d)))

;;; 8.60 d write grandparents, a function that returns the set of a persons grandparents, use mapunion

(assert (equal (grandparents 'tamara) '(kate arthur colin deidre)))

;;; 8.60 e write cousins (children of any of their parents siblings), use mapunion

(assert (equal (cousins 'julie) '(vincent tamara nigel)))

;;; 8.60 f write the two-input recursive predicate descended-from that returns a true value if the first person is descended from the second

(assert (descended-from 'tamara 'arthur))
(assert (not (descended-from 'tamara 'linda)))

;;; 8.60 g write the recursive function ancestors that returns a person's set of ancestors

(assert (equal (ancestors 'marie) '(george ellen linda frank arthur kate)))

;;; 8.60 h write a recursive function generation-gap that returns the number of generations seperating a person and one of his or her ancestors

(assert (= (generation-gap 'suzanne 'colin) 1))
(assert (= (generation-gap 'frederick 'colin) 3))
(assert (not (generation-gap 'frederick 'linda)))

;;; 8.60 i

;; is robert descended from deidre

;; who are yvette's ancestors

;; what is the generation gap between olivia and frank

;; who are peter's cousins

;; who are olivias grandparents

;;; 8.61 write a tail-recursive version of count-up

(assert (equal (count-up 5) '(1 2 3 4 5)))
(assert (equal (count-up 0) nil))

;;; 8.62 write a tail-recursive version of fact

(assert (= (my-fact 3) 6))

;;; 8.63 write tail-recursive versions of union, intersection, and set-difference

(assert (equal (my-union '(a b c) '(a c d)) '(d c b a)))
(assert (equal (my-union '(a b) '(c)) '(c b a)))

(assert (equal (my-intersection '(a b c) '(b)) '(b)))
(assert (equal (my-intersection '(a b c) '(d e f)) '()))
(assert (equal (my-intersection '(a b c) '()) '()))

(assert (equal (my-set-difference '(a b c d) '(b d)) '(c a)))
(assert (equal (my-set-difference '(a b c d) '()) '(d c b a)))
(assert (equal (my-set-difference '() '(a b c d)) '(a b c d)))

;;; 8.64 write a tree-find-if operator that returns the first non-nil atom of a tree that satisfies a predicate

(assert (equal (tree-find-if #'oddp '((2 4) (5 6) 7)) 5))
(assert (equal (tree-find-if #'oddp '((2 4) (6 5) 7)) 5))

;;; 8.65 use labels to write versions of tr-count-slices and tr-reverse

(assert (= (tr-count-slices '(x x x)) 3))

(assert (equal (tr-reverse '(a b c)) '(c b a)))

;;; 8.66 write arith-eval, a function that evaluates arithmetic expressions

(assert (= (arith-eval '(1 + 1)) 2))
(assert (= (arith-eval '((2 * 2)) 4)))
(assert (= (arith-eval '(2 + (3 * 4))) 14))

;;; 8.67 write a predicate legalp that returns t if its input is a legal arithmetic expression

(assert (legalp 4))
(assert (legalp '((2 * 2) - 3)))
(assert (not (legalp nil)))
(assert (not (legalp '(a b c d))))

;;; 8.69 write a function that can generate the factorisation tree for calculating primes
;;; nb slightly different than 69-70 in the book

(assert (equal (primes 60) '(2 2 3 5)))
