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

(assert (equal (cadr (assoc 'pennsylvania revised-atlas)) '(pittsburgh johnstown)))
(assert (equal (cadr (assoc 'new-jersey revised-atlas)) '(newark princeton trenton)))
(assert (equal (cadr (assoc 'ohio revised-atlas)) '(columbus)))

;;; 6.35 the nerd has five states: sleeping, eating, waiting-for-a-computer, programming, debugging. Behaviour is cyclic.

;;; 6.35 a write a data structure that contains the five states above, each representing the connection between a state and its successor. Store in nerd-states.

(defvar nerd-states
  '((sleeping . eating)
    (eating . waiting-for-a-computer)
    (waiting-for-a-computer . programming)
    (programming . debugging)
    (debugging . sleeping)))

;;; 6.35 b write a function nerdus that takes the name of a state as input and uses the above data structure to determine the next state the creature will be in.

(defun nerdus (current)
  (cdr (assoc current nerd-states)))

(assert (equal (nerdus 'sleeping) 'eating))
(assert (equal (nerdus 'debugging) 'sleeping))

;;; 6.35 c what is the result of
(nerdus 'playing-guitar) ; nil

;;; 6.35 d when nerdus ingests too many stimulants, it stops sleeping. After finishing debugging, it immediately goes on to state 'eating. Write a function sleepless-nerd that works just like nerdus except it never sleeps, it should use nerd-states.

(defun sleepless-nerd (current)
  (let ((next-state (nerdus current)))
    (if (equal next-state 'sleeping)
	(nerdus next-state)
	next-state)))

(assert (equal (sleepless-nerd 'debugging) 'eating))
(assert (equal (sleepless-nerd 'waiting-for-a-computer) 'programming))

;;; 6.35 e nerd can jump two states when on stimulants, write a function that exhibits this pathology, it should use nerd-states

(defun nerd-on-caffeine (current)
  (nerdus (nerdus current)))

(assert (equal (nerd-on-caffeine 'sleeping) 'waiting-for-a-computer))
(assert (equal (nerd-on-caffeine 'waiting-for-a-computer) 'debugging))

;;; 6.35 f if a nerd on caffeine is currently programming, how many states will it have to go through before it is debugging?

(defun until-expected-state (current expected &optional (jumps (list current)))
  "tracks the set of states nerdus goes through until reaching the expected state"
  (let* ((next-state (nerd-on-caffeine current))
	 (jumps (cons next-state jumps)))
    (if (equal next-state expected)
	(reverse jumps)
	(until-expected-state next-state expected jumps))))

(assert (equal (until-expected-state 'sleeping 'waiting-for-a-computer) '(sleeping waiting-for-a-computer)))
(assert (equal (until-expected-state 'sleeping 'debugging) '(sleeping waiting-for-a-computer debugging)))
(assert (equal (until-expected-state 'programming 'debugging) '(programming sleeping waiting-for-a-computer debugging)))

;;; 6.36 write a function that swaps the first and last elements of any list

(defun swap-first-last (ls)
  (if (null (rest ls)) '()
      (let ((start (first ls))
	    (end (car (last ls))))
	(reverse
	 (cons start
	       (rest (reverse (cons end (rest ls)))))))))

(assert (equal (swap-first-last '()) '()))
(assert (equal (swap-first-last '(you cant buy love)) '(love cant buy you)))

;;; 6.37 rotate-left and rotate-right are functions that rotate elements of a list.

(defun rotate-left (ls)
  (append (rest ls) (list (first ls))))

(defun rotate-right (ls)
  (let ((rev (reverse ls)))
    (cons
     (car rev)
     (reverse (rest rev)))))

(assert (equal (rotate-left '(A B C D E)) '(B C D E A)))
(assert (equal (rotate-right '(A B C D E)) '(E A B C D)))

;;; 6.41 map of room

(defvar rooms
  '((living-room
     (north front-stairs)
     (south dining-room)
     (east kitchen))
    (upstairs-bedroom
     (west library)
     (south front-stairs))
    (dining-room
     (north living-room)
     (east pantry)
     (west downstairs-bedroom))
    (kitchen
     (west living-room)
     (south pantry))
    (pantry
     (north kitchen)
     (west dining-room))
    (downstairs-bedroom
     (north back-stairs)
     (east dining-room))
    (back-stairs
     (south downstairs-bedroom)
     (north library))
    (front-stairs
     (north upstairs-bedroom)
     (south living-room))
    (library
     (east upstairs-bedroom)
     (south back-stairs))))

;;; 6.41 a write a function choices that takes the name of a room and returns permissable directions

(defun choices (location)
  "lists the available locations accessible from this location"
  (rest (assoc location rooms)))

(assert (equal (choices 'pantry) '((north kitchen) (west dining-room))))

;;; 6.41 b write a function look that takes two inputs, a direction and a room, and tells where robbie would end up if he moved in that direction from the room.

(defun look (direction location)
  "returns the location that can be accessed via current location and direction"
  (second (assoc direction (choices location))))

(assert (equal (look 'north 'pantry) 'kitchen))
(assert (equal (look 'south 'pantry) nil))

;;; 6.41 c a global variable loc will hold robbie's location. Write an expression that sets Robbie's location.

(defvar *loc*)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC"
  (setf *loc* place))

(progn
  (set-robbie-location 'kitchen)
  (assert (equal *loc* 'kitchen)))

;;; 6.41 d write a function how-many-choices that counts the amount of exits available from the current location
;;; your function should refer to the global variable loc to find robbie's current location

(defun how-many-choices ()
  (length (choices *loc*)))

(progn
  (set-robbie-location 'pantry)
  (assert (= (how-many-choices) 2)))

;;; 6.41 e write a predicate upstairsp that returns t if its input is an upstairs location (library / upstairs-bedroom)
;;; write a predicate onstairsp that returns t if input is either front-stairs or back-stairs

(defun upstairsp (location)
  (member location '(library upstairs-bedroom)))

(defun onstairsp (location)
  (member location '(front-stairs back-stairs)))

(assert (upstairsp 'library))
(assert (upstairsp 'upstairs-bedroom))
(assert (not (upstairsp 'front-stairs)))

(assert (onstairsp 'front-stairs))
(assert (onstairsp 'back-stairs))
(assert (not (onstairsp 'library)))

;;; 6.41 f where's robbie? Write a function called where that tells where robbie is.

(defun where ()
  "describes the current location of Robbie"
  (append
   '(robbie is)
   (cond
     ((upstairsp *loc*) '(upstairs in the))
     ((onstairsp *loc*) '(on the))
     (t '(downstairs in the)))
   (list *loc*)))

(progn
  (set-robbie-location 'library)
  (assert (equal (where) '(robbie is upstairs in the library))))
(progn
  (set-robbie-location 'kitchen)
  (assert (equal (where) '(robbie is downstairs in the kitchen))))
(progn
  (set-robbie-location 'front-stairs)
  (assert (equal (where) '(robbie is on the front-stairs))))

;;; 6.41 g write move that takes direction as input, and moves robbie in that direction.

(defun move (direction)
  (let ((moved (look direction *loc*)))
    (cond
      ((null moved) '(ouch! robbie hit a wall))
      (t (set-robbie-location moved)
	 (where)))))

(progn
  (set-robbie-location 'pantry)
  (assert (equal (move 'south) '(ouch! robbie hit a wall))))
(progn
  (set-robbie-location 'pantry)
  (assert (equal (move 'north) '(robbie is downstairs in the kitchen))))

;;; 6.41 h starting from the pantry, take robbie to the library via the back stairs.
;;; Then take him to the kitchen, but do not lead him through the downstairs bedroom on the way

(defun moves (directions &optional (path (list (where))))
  "moves robbie across a set of rooms by directions, returning the room descriptions as he passes through"
  (if (null directions) (reverse path)
      (let* ((moved (move (car directions)))
	     (path (cons moved path)))
	(cond
	  ((equal (first moved) 'ouch!) (reverse path))
	  (t (moves (rest directions) path))))))

(progn
  (set-robbie-location 'pantry)
  (moves '(west west north north)))
(progn
  (set-robbie-location 'library)
  (moves '(east south south east south)))
