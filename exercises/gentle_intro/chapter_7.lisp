;;; 7.1 write an add1 function that adds one to its input, then write a function to add one to each element of the list.

(assert (equal (add1s '(13 5 7 9)) '(14 6 8 10)))
(assert (equal (add1s '()) '()))

;;; 7.2 use mapcar on the below table to extract a list of social security numbers

(defvar daily-planet
  '((olsen jimmy 123-76-4535 cub-reporter)
    (kent clark 089-52-6787 reporter)
    (lane lois 951-26-1438 reporter)
    (white perry 355-16-7439 editor)))

(assert (equal (social-securities)
	       '(123-76-4535 089-52-6787 951-26-1438 355-16-7439)))

;;; 7.3 write a function to apply the zerop predicate to the following input

(assert (equal (check-zeroes '(2034 0 -5 -6)) '(nil t nil nil)))

;;; 7.4 repeat 7.3, but this time check if the input is greater than five.
;;; define a predicate greater-than-five-p to help.

(assert (equal (greater-than-fives '(2034 0 -5 -6)) '(t nil nil nil)))

;;; 7.5 write a lambda expression to subtract seven from a number

(defvar sub-7)

(assert (= (funcall sub-7 10) 3))
(assert (= (funcall sub-7 0) -7))

;;; 7.6 write a lambda expression that returns t if its input is t or nil, but nil for any other input.

(defvar t-or-nil)

(assert (funcall t-or-nil t))
(assert (funcall t-or-nil nil))
(assert (not (funcall t-or-nil 0)))
(assert (not (funcall t-or-nil 'true)))

;;; 7.7 write a function that takes a list below and "flips" each element.
;;; Your function should include a lambda expression that flips an individual element.
;;; It should also include an applicative operator to do this to every element of the list.

(assert (equal (flips '(up down up up)) '(down up down down)))

;;; 7.8 write a function that takes two inputs, x and k, and returns the first number in the list x that is roughly equal to k (between K-10 and K+10)

(assert (= (roughly-10 '(110 20 30 -15 8 25) 5) 8))
(assert (= (roughly-10 '(11 20 -3 -15 15 25) -8) -3))

;;; 7.9 write a function find-nested that returns the first element of a list that is itself a non-nil list

(assert (equal (find-nested '(() () nil (1 2 3) nil)) '(1 2 3)))
(assert (equal (find-nested '(())) nil))

;;; 7.10 a write a table storing the following information in a global variable note-table:
;;; (this is stated in c, but it can't use dotted pairs, although the back of the book solution uses dotted pairs)
;;; C = 1, C-Sharp = 2, D = 3, D-Sharp = 4, E = 5, F = 6, F-Sharp = 7, G = 8, G-Sharp = 9, A = 10, A-Sharp = 11, B = 12

(defvar note-table
  '((c 1)
    (c-sharp 2)
    (d 3)
    (d-sharp 4)
    (e 5)
    (f 6)
    (f-sharp 7)
    (g 8)
    (g-sharp 9)
    (a 10)
    (a-sharp 11)
    (b 12)))

;;; 7.10 b write a function numbers that takes a list of notes as input and returns the corresponding list of numbers

(assert (equal (numbers '(E D C D E E E)) '(5 3 1 3 5 5 5)))

;;; 7.10 c write a function notes that takes a list of number as input and returns the corresponding list of notes.

(assert (equal (notes '(5 3 1 3 5 5 5)) '(E D C D E E E)))

;;; 7.10 d as the above two functions are mutual inverses

(equal (notes (numbers '(A B C))) '(A B C))
(equal (numbers (notes '(1 2 3))) '(1 2 3))

;; what do the following do? 
(notes (notes '(1 2 3))) 
(numbers (numbers '(A B C)))

;;; 7.10 e write a function raise that takes a number n and a list of numbers as input and raises each number in list by n

(assert (equal (raise 5 '(5 3 1 3 5 5 5)) '(10 8 6 8 10 10 10)))

;;; 7.10 f write a function normalise, that wraps the list of numbers to match a number in the note table
;;; i.e. all numbers should be between 1 and 12

(assert (equal (normalise '(6 10 13)) '(6 10 1)))
(assert (equal (normalise '(-5 10 13)) '(5 10 1)))
(assert (equal (normalise '(-15 10 13)) '(9 10 1)))

;;; 7.10 g write a function transpose, that takes a number n and a song as input, and returns the song transposed by n half steps
;;; numbers, notes, raise, normalise should be considered

(assert (equal (transpose 5 '(E D C D E E E)) '(A G F G A A A)))
(assert (equal (transpose 11 '(E D C D E E E)) '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)))
(assert (equal (transpose 12 '(E D C D E E E)) '(E D C D E E E)))
(assert (equal (transpose -1 '(E D C D E E E)) '(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)))

;;; 7.11 write a function that picks out numbers from a list that are greater than one and less than five

(assert (equal (gt1-lt5 '(1 2 3 4 5)) '(2 3 4)))

;;; 7.12 write a function that counts how many times "the" appears in a sentence

(assert (= (count-the '(a the another the)) 2))

;;; 7.13 write a function that picks from a list of lists those of exactly length two

(assert (equal (2len-lists '((1) (1 2) (1 2 3) (4 5))) '((1 2) (4 5))))

;;; 7.14 below is set-difference using remove-if
;;; implement intersection and union using remove-if or remove-if-not

(defun my-setdiff (x y)
  (remove-if #'(lambda (e) (member e y))
	     x))

(assert (equal (my-intersection '(1 2 3) '(2 3 4)) '(2 3)))

(assert (equal (my-union '(1 2 3) '(2 3 4)) '(1 2 3 4)))
   
;;; 7.15 a write the functions rank and suit that return the rank and suit of a card

(defvar my-hand
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(assert (= (rank '(2 clubs)) 2))
(assert (equal (suit '(2 clubs)) 'clubs))

;;; 7.15 b write a function count-suit that takes two inputs, a suit and a hand of cards, and returns the number of cards belonging to that suit

(assert (= (count-suit 'diamonds my-hand) 2))

;;; 7.15 c write a function colour-of that uses the table colours to retrieve the colour of a card

(defvar colours
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))

(assert (equal (color-of '(6 hearts)) 'red))

;;; 7.15 d write a function first-red that returns the first card of a hand that is of a red suit, or nil if none are

(assert (equal (first-red my-hand) '(3 hearts)))
(assert (equal (first-red (reverse my-hand)) '(4 diamonds)))

;;; 7.15 e write a function black-cards that returns a list of all the black cards in a hand

(assert (equal (black-cards my-hand) '((5 clubs) (ace spades))))

;;; 7.15 f write a function what-ranks that takes two inputs, a suit and a hand, and returns the ranks of all cards belonging to that suit

(assert (equal (what-ranks 'diamonds my-hand) '(2 4)))
(assert (equal (what-ranks 'spades my-hand) '(ace)))

;;; 7.15 g write a predicate higher-rank-p that takes two cards as input and returns true if the first card has a higher rank than the second

(assert (higher-rank-p '(3 diamonds) '(2 spades)))
(assert (higher-rank-p '(queen spades) '(2 diamonds)))
(assert (not (higher-rank-p '(3 clubs) '(jack spades))))

;;; 7.15 h write a function high-card that returns the highest ranked card in a hand

(assert (equal (high-card-reduce my-hand) '(ace spades)))
(assert (equal (high-card-reduce (reverse my-hand)) '(ace spades)))
(assert (equal (high-card-find-if my-hand) '(ace spades)))
(assert (equal (high-card-find-if (reverse my-hand)) '(ace spades)))

;;; 7.16 suppose we had a list of sets as below, if append is used for the appending function it won't be a true set, what reducing function should be used?

(defvar non-unique
  '((A B C) (C D A) (F B D) (G)))

(assert (equal (sort (my-combine non-unique) #'string<) '(A B C D F G)))

;;; 7.17 write a function that, given a list of lists, returns the total length of all lists
;;; it can be solved in two ways, identify them (hint: append is likely the one you aren't thinking about)

(assert (= (total-length '((a) (a b) (a b c))) 6))

;;; 7.19 write a function all-odd that returns t if every element of a list of numbers is odd

(assert (all-odd '(1 3 7)))
(assert (not (all-odd '(1 2 3))))

;;; 7.20 write a function none-odd that returns t if every element of a list of numbers is not odd

(assert (none-odd '(2 4)))
(assert (not (none-odd '(1 2 4))))

;;; 7.21 write a function not-all-odd that returns t if not every element of a list of numbers is odd

(assert (not-all-odd '(1 2 3)))
(assert (not (not-all-odd '(1 3))))

;;; 7.22 write a function not-none-odd that returns t if it is not the case that a list of numbers contains no odd elements

(assert (not-none-odd '(1 2 3)))
(assert (not-none-odd '(1 3)))
(assert (not (not-none-odd '(2 4))))

;;; 7.26 show how to write find-if given remove-if-not

(assert (equal (my-find-if #'oddp '(2 4 5 6)) 5))
(assert (equal (my-find-if #'oddp '(2 4 6)) nil))

;;; 7.27 show how to write every given remove-if

(assert (my-every #'oddp '(1 3 7)))
(assert (not (my-every #'oddp '(1 3 4 7))))

;;; 7.29 rules

(defvar database
 '((b1 shape brick)
   (b1 colour green)
   (b1 size small)
   (b1 supported-by b2)
   (b1 supported-by b3)
   (b2 shape brick)
   (b2 colour red)
   (b2 size small)
   (b2 supports b1)
   (b2 left-of b3)
   (b3 shape brick)
   (b3 colour red)
   (b3 size small)
   (b3 supports b1)
   (b3 right-of b2)
   (b4 shape pyramid)
   (b4 colour blue)
   (b4 size large)
   (b4 supported-by b5)
   (b5 shape cube)
   (b5 colour green)
   (b5 size large)
   (b5 supports b4)
   (b6 shape brick)
   (b6 colour purple)
   (b6 size large)))

;;; 7.29 a write a function match-element that takes two symbols as inputs and returns if they are equal
;;; if the second parameter is a question mark, match-element returns t

(assert (match-element 'red 'red))
(assert (match-element 'red '?))
(assert (not (match-element 'red 'blue)))

;;; 7.29 b write a function match-triple that takes an assertion and a pattern input, and returns t if the assertion matches the pattern

(assert (match-triple '(b2 colour red) '(b2 colour ?)))
(assert (match-triple '(b2 colour red) '(b2 colour red)))
(assert (not (match-triple '(b2 colour red) '(b1 colour green))))

;;; 7.29 c write the function fetch that takes a pattern as input and returns all assertions in the database that match the pattern

(assert (equal (fetch '(b2 colour ?)) '((b2 colour red))))
(assert (equal (fetch '(? supports b1)) '((b2 supports b1) (b3 supports b1))))

;;; 7.29 d use fetch with patterns to answer the following questions

;; what shape is block b4?
(fetch '(b4 shape ?)) 

;; which blocks are bricks?
(fetch '(? shape brick)) 

;; what relation is block b2 to block b3?
(fetch '(b2 ? b3)) 

;; list the colour of every block
(fetch '(? colour ?))

;; what facts are known about block b4
(fetch '(b4 ? ?)) 

;;; 7.29 e write a function that takes a block name as input and returns a /pattern/ asking the colour of the block

(assert (equal (colour-pattern 'b5) '(b5 colour ?)))

;;; 7.29 f write a function supporters that takes one input, a block, and returns a list of all blocks that support it

(assert (equal (supporters 'b1) '(b2 b3)))

;;; 7.29 g write a predice supp-cube that takes a block as input and returns true if that block is supported by a cube

(assert (supp-cube 'b4))
(assert (not (supp-cube 'b1)))

;;; 7.29 h write a desc1 function that returns all assertions dealing with a block

(assert (equal (desc1 'b6) '((b6 shape brick) (b6 colour purple) (b6 size large))))

;;; 7.29 i write a function desc2 that calls desc1 and strips the block name off each element of the result

(assert (equal (desc2 'b6) '((shape brick) (colour purple) (size large))))

;;; 7.29 j write the description function, it should take one input, call desc2, and merge the resulting list of lists into a single list

(assert (equal (description 'b6) '(shape brick colour purple size large)))
(assert (equal (description 'b2) '(shape brick colour red size small supports b1 left-of b3)))

;;; 7.29 k what is the description of b1, b4?

(description 'b1)
(description 'b4)

;;; 7.29 block b1 is made of wood, but block b2 is made of plastic, how would you add this information to the database?

;;; 7.30 recall the english-french dictionary earlier, given this dictionary plus the list of corresponding spanish words (below), write an expression to return a trilingual dictionary

(defvar words '((one un) (two deux) (three trois) (four quatre) (five cinq)))
(defvar spanish '(uno dos tres quatro cinco))

(assert (equal (assoc 'one tri) '(one un uno)))
