(load "common.lisp")

;;; 6.1 why does the following equal nil?

(nth 4 '(A B C)) 

;;; 6.2 what is the value of the following and why.

(nth 3 '(A B C . D)) 

;;; 6.3 what is the value of

(last '(rosebud)) 

;;; 6.4 what is the value of the following and why.

(last '((A B C))) 

;;; 6.5 based on the following variable, write down what each of these expressions evalutes to

(defvar line '(roses are red))

(reverse line) 

(first (last line)) 

(nth 1 line) 

(reverse (reverse line)) 

(append line (list (first line))) 

(append (last line) line) 

(list (first line) (last line)) 

(cons (last line) line) 

(remove 'are line) 

(append line '(violets are blue)) 

;;; 6.6 (1) use last function to write a function called last-element that returns the last element of a list instead of the last cons cell.

(assert (equal (last-element '(1 2 3)) 3))
(assert (equal (last-element '(1)) 1))

;;; 6.6 (2) write another version of last-element using reverse instead of last.

(assert (equal (last-element-reverse '(1 2 3)) 3))
(assert (equal (last-element-reverse '(1)) 1))

;;; 6.6 (3) write another version using nth and length

(assert (equal (last-element-nth '(1 2 3)) 3))
(assert (equal (last-element-nth '(1)) 1))

;;; 6.7 (1) use reverse to write a next-to-last function that returns the next-to-last element of a list.

(assert (equal (next-to-last '(1 2 3 4 5)) 4))
(assert (equal (next-to-last '(1 2)) 1))
(assert (equal (next-to-last '(1)) nil))

;;; 6.7 (2) write another version using nth

(assert (equal (next-to-last-nth '(1 2 3 4 5)) 4))
(assert (equal (next-to-last-nth '(1 2)) 1))
(assert (equal (next-to-last-nth '(1)) nil))

;;; 6.8 write a function my-butlast that returns a list with the last element removed

(assert (equal (my-butlast '(roses are red)) '(roses are)))
(assert (equal (my-butlast '(G A G A)) '(G A G)))

;;; 6.9 what primitive function does the following reduce to (what is it the equivalent of)?

(defun mystery (x) (first (last (reverse x)))) 

;;; 6.10 a palindrome is a sequence that reads the same forwards and backwards. Write a function palindromep that returns t if its input is a palindrome.

(assert (palindromep '(A B C D C B A)))
(assert (not (palindromep '(A B C A B C))))

;;; 6.11 write a function make-palindrome that makes a palindrome out of a list

(assert (equal (make-palindrome '(you and me)) '(you and me me and you)))
(assert (equal (make-palindrome '(1)) '(1 1)))

;;; 6.13 what is the result of intersecting a set with nil?

(intersection '(1 2 3) nil) 

;;; 6.14 what is the result of intersecting a set with itself?

(defvar sa '(1 2 3))
(intersection sa sa) 

;;; 6.15 we can use member to write a predicate that returns a true value if a sentence contains the word "the"

(defun contains-the-p (sent)
  (member 'the sent))

;; suppose we want a predicate contains-article-p that returns a value if a sentence contains "the", "a", "an", write a version using intersection.

(assert (contains-article-p '(test the tester)))
(assert (contains-article-p '(test a tester)))
(assert (not (contains-article-p '(test tester))))

;; write a version using member and or

(assert (contains-article-mem-p '(test the tester)))
(assert (contains-article-mem-p '(test a tester)))
(assert (not (contains-article-mem-p '(test tester))))

;; can this be solved with and instead of or?

(assert (contains-article-and-p '(test the tester)))
(assert (contains-article-and-p '(test a tester)))
(assert (not (contains-article-and-p '(test tester))))

;;; 6.16 what is the union of a set with nil?

(union '(1 2 3) nil) 

;;; 6.17 with is the union of a set with itself?

(defvar ua '(1 2 3))
(union ua ua) 

;;; 6.18 write a function add-vowels that takes a set of letters as input andds the vowels '(a e i o u) to the set.

(assert (equal (sort (add-vowels '(X A E Z)) #'string<) (sort '(X A E Z I O U) #'string<)))
(assert (equal (sort (add-vowels '()) #'string<) '(A E I O U)))

;;; 6.19 what are the results of using nil as an input to set-difference?

(set-difference '(1 2 3) nil) 
(set-difference nil '(1 2 3))

;;; 6.21 if set x is a subset of set y, then subtracting y from x should leave the empty set. Write my-subsetp a version of subsetp that returns t if its first input is a subset of its second input.

(assert (my-subsetp '(1 2 3) '(1 2 3 4)))
(assert (not (my-subsetp '(1 2 3) '(1 2 4))))

;;; 6.22 suppose the following variable, what will be the result of each of the following expressions

(defvar seta '(soap water))

(union seta '(no soap radio)) 

(intersection seta (reverse seta)) 

(set-difference seta '(stop for water)) 

(set-difference seta seta) 

(member 'soap seta) 

(member 'water seta) 

(member 'washcloth seta) 

;;; 6.24 sets are said to be equal if they contain exactly the same elements, even if in a different order. Write a set-equal predicate that returns t if two things are equal as sets.

(assert (set-equal '(red blue green) '(green blue red)))
(assert (not (set-equal '(red blue green) '(red blue yellow))))

;;; 6.25 a set x is a proper subset of set y if x is a subset of y but not equal to y. Write the proper-subsetp predicate, which returns t if its first input is a proper subset of its second input.

(assert (proper-subsetp '(A C) '(A B C)))
(assert (not (proper-subsetp '(C A B) '(A B C))))

;;; 6.26 set of functions that compare descriptions of two objects and tell how many features they have in common
;;; i.e. (large red shiny cube -vs- small shiny red four-sided pyramid) should respond with (2 common features)

(defvar object-example '(large red shiny cube -vs- small shiny red four-sided pyramid))

;;; 6.26 a write a function right-side that returns all the features to the right of the -vs- symbol

(assert (equal (right-side object-example) '(small shiny red four-sided pyramid)))

;;; 6.26 b write a function left-side that returns all the features to the left of the -vs-

(assert (equal (left-side object-example) '(large red shiny cube)))

;;; 6.26 c write a function count-common that returns the number of features the left and right sides have in common

(assert (= (count-common (left-side object-example)
			 (right-side object-example))
	   2))

;;; 6.26 d write the main function, compare, that takes a list of features describing two objects, with a -vs- between them, and reports the number of features they have in common

(assert (equal (compare object-example)
	       '(2 common features)))
(assert (equal (compare '(small red metal cube -vs- red plastic small cube))
	       '(3 common features))) ; 6.26 e

;;; 6.28 based on the following list, what do the expressions evalute to?

(defvar produce
  '((apple . fruit)
    (celery . veggie)
    (banana . fruit)
    (lettuce . veggie)))

(assoc 'banana produce) ; finds banana . fruit
(rassoc 'fruit produce) ; finds apple . fruit (first matching element)
(assoc 'lettuce produce) ; finds lettuce . veggie
(rassoc 'veggie produce) ; finds celery . veggie

;;; 6.30 make a table called books of five books and their authors, the first entry might be (war-and-peace leo-tolstoy)

(defvar books
  '((war-and-peace leo-tolstoy)
    (complete-works plato)
    (computer-organisation-and-design patterson-hennessey)
    (on-sparta plutarch)
    (letters seneca)))

;;; 6.31 write the function who-wrote that takes the name of a book as input and returns the book's author

(defun who-wrote (name)
  (second (assoc name books)))

(assert (equal (who-wrote 'letters) 'seneca))
(assert (equal (who-wrote 'on-sparta) 'plutarch))

;;; 6.32 suppose we do (setf books (reverse books)) which reverse the order in which the five books appear in the table. What will the who-wrote function do now?

;; nothing different, not order dependent.
(setf books (reverse books))
(assert (equal (who-wrote 'letters) 'seneca))

;;; 6.33 suppose we wanted a what-wrote function that took an author's name as input and return the title of one of his or her books, could we create such a function using assoc and the current table? If not, how would the table have to be different?

;;; we could rewrite using dotted pairs, or use a map that flips the order around.

;;; 6.34 here is a table of states and some of their cities, stored in the global variable atlas

(defvar atlas
  '((pennsylvania pittsburgh)
    (new-jersey newark)
    (pennsylvania johnstown)
    (ohio columbus)
    (new-jersey princeton)
    (new-jersey trenton)))
	       
;; suppose we wanted to find all the cities a given state contains. Redesign this list so that assoc can be used.

(defvar revised-atlas
  '((pennsylvania (pittsburgh johnstown))
    (ohio (columbus))
    (new-jersey (newark princeton trenton))))

(assert (same-elements (cadr (assoc 'pennsylvania revised-atlas)) '(pittsburgh johnstown)))
(assert (same-elements (cadr (assoc 'new-jersey revised-atlas)) '(newark princeton trenton)))
(assert (same-elements (cadr (assoc 'ohio revised-atlas)) '(columbus)))
