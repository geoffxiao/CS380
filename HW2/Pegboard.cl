; Geoffrey Xiao
; CS 380
; 
;
; Welcome to GNU CLISP 2.49 (2010-07-07) <http://clisp.cons.org/>
; 
; Done on tux
;	 Linux version 4.4.0-116-generic (buildd@lgw01-amd64-021) (gcc version 5.4.0
;	 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.9) ) ;140-Ubuntu SMP Mon Feb 12
;	 21:23:04 UTC 2018


; represent m x n board as a list
; m = 3, n = 4
;
; 11 10 9 8
;  7  6 5 4
;  3  2 1 0
;
; Board = [a0, a1, ...]
; 1 = peg is occupied
; 0 = peg is not occupied
;
; Peg index starts from 0!

;; ---------------------------------------------------------
;; ---------------------------------------------------------
;; Global Constants

(defvar finalPos 9) ; empty peg here, 9th peg is empty
(defvar m 4) ; number of rows
(defvar n 4) ; number of columns

;; ---------------------------------------------------------
;; ---------------------------------------------------------

; Create initial state for m x n board
(defun initialStateHelper (m n i)
	(cond ((= i (* m n))	'()) ; all pegs updated
			((= i finalPos) (cons 0 (initialStateHelper m n (+ i 1)))) ; empty peg
			(T (cons 1 (initialStateHelper m n (+ i 1)))))) ; continue recursion

(defvar initialState (initialStateHelper m n 0)) ; initial state

; Has the goal been reached?
(defun goal (state)
	(if (and (= (apply '+ state) 1) (nth finalPos state)) ; 1 peg filled and peg in 9th pos
		T
		NIL))

; Apply rule to a given state
; Return new state that is the result of the rule application
(defun applyRule (rule state)
	(let ((jumper (first rule))
			(goner (second rule))
			(newpos (third rule)))
		(applyRuleHelper jumper goner newpos 0 state)))

; Helper function for applying the rule
(defun applyRuleHelper (jumper goner newpos i state)
	(cond ((= i (* m n)) '()) ; all pegs updated
			((or (= i jumper) (= i goner)) (cons 0 (applyRuleHelper jumper goner newpos (+ i 1) state))) ; jumper/goner, peg = 0
			((= i newpos) (cons 1 (applyRuleHelper jumper goner newpos (+ i 1) state))) ; newpos, peg = 1
			(T (cons (nth i state) (applyRuleHelper jumper goner newpos (+ i 1) state))))) ; else, get peg status from state

; row of the cell
(defun row (cell)
	(floor cell n))

; column of the cell
(defun col (cell)
	(mod cell n))

; Are jumper, goner, newpos left/right of one another?
(defun precondLeftRight (rule state)
	(let ((jumper (first rule))
		(goner (second rule))
		(newpos (third rule)))
		(and (= (abs (- jumper goner)) 1) (= (abs (- goner newpos)) 1)
			(= (row jumper) (row goner))
			(= (row goner) (row newpos))
			(= (- jumper goner) (- goner newpos)))))


; Are jumper, goner, newpos up/down of one another?
(defun precondUpDown (rule state)
	(let ((jumper (first rule))
		(goner (second rule))
		(newpos (third rule)))
		(and (= (abs (- jumper goner)) n) (= (abs (- goner newpos)) n)
			(= (- jumper goner) (- goner newpos)))))

; Are jumper, goner, newpos diagonal of one another?
(defun precondDiag (rule state)
	(let ((jumper (first rule))
		(goner (second rule))
		(newpos (third rule)))
		(and (= (abs (- (col jumper) (col goner))) 1)
			(= (- (col jumper) (col goner)) (- (col goner) (col newpos)))
			(= (abs (- (row jumper) (row goner))) 1)
			(= (- (row jumper) (row goner)) (- (row goner) (row newpos))))))

; Is the rule allowed in state?
(defun precondition (rule state)
	(let ((jumper (first rule))
		(goner (second rule))
		(newpos (third rule)))
		(and (or (precondLeftRight rule state) (precondUpDown rule state)
			(precondDiag rule state))
			(= (nth jumper state) 1) (= (nth goner state) 1) 
			(= (nth newpos state) 0))))

; Get all moves, (0, ..., m*n - 1)
(defun movesHelper (i)
	(if (= i (* m n)) 
		'()
		(cons i (movesHelper (+ 1 i)))))

(defvar moves (movesHelper 0))

; Generate all the possible moves
; (m*n)^3 possible moves
; Cartesian product of (0, ..., m*n -1), (0, ..., m*n -1), (0, ..., m*n -1)
; Code from https://gist.github.com/capablemonkey/4133438ba7043af94691a2b54d997e8b
(defun cartesian-product (a b)
  (mapcan
    (lambda (item-from-a)
      (mapcar
        (lambda (item-from-b)
          (if (listp item-from-a)
            (append item-from-a (list item-from-b))
            (list item-from-a item-from-b)))
        b))
    a))

; All possible combination of (jumper, goner, newpos) moves
(defvar allMoves (reduce #'cartesian-product (cons moves (cons moves (cons moves '())))))

; Get the applicable moves
(defun applicableMoves (state)
	(remove-if-not (lambda (x) (precondition x state)) allMoves))

; Print the Peg Board
(defun describeStateHelper (state i)
	(let ((col (mod i n)))
		(if (= i (* m n))
			(format t "~%")
			(progn 
				(if (= col 0)
					(format t "~a~%" #\Space)	
					t)	
				(if (= (nth i state) 1)
					(format t "X~a" #\Space)
					(format t "O~a" #\Space))
				(describeStateHelper state (+ i 1))))))

; Print the Peg Board state
(defun describeState (state) (describeStateHelper (reverse state) 0))

; Describe a rule
(defun describeRule (rule)
	(format t "Peg ~a jumps over Peg ~a and lands in Slot ~a~%" 
		(first rule) (second rule) (third rule)))

; Select a random element from a list
(defun randomEl (el) 
	(nth (random (length el)) el))

; Flail Wildy algorithm
(defun flailWildly (state i)
	(let ((availMoves (applicableMoves state)))
		(cond 
			((goal state) 
				(progn 
					(format t "~%State ~a:" i)
					(describeState state)
					(format t "~%======================================")
					(format t "~%Success!~%")))
			((null availMoves)
				(progn
					(format t "~%State ~a:" i)
					(describeState state)
					(format t "~%======================================")
					(format t "~%Failure~%")))
			(t (let ((chosenMove (randomEl availMoves)))
					(progn
						(format t "~%State ~a:~%" i)
						(describeState state)
						(format t "~%Allowed Moves:~%")
						(map 'NIL #'describeRule availMoves)
						(format t "~%Chosen Move: ")
						(describeRule chosenMove)
						(format t "======================================")
						(flailWildly (applyRule (first availMoves) state) (+ 1 i))))))))

; Flail!
; (flailWildly initialState 0)

(defvar total 0)
(defun backTrack (stateList depthBound)
	(setq s (first stateList))
	(setq ruleSet (applicableMoves s))
	(setq total (+ 1 total))
	(cond ((goal s) 'NIL)
			((or (member s (rest stateList) :test 'equal) (> (list-length stateList) depthBound) 
	 		(null ruleSet))
					(progn (print 'Failed) 'Failed))
			(T (loop for r in ruleSet
				do (setq newState (applyRule r s)) 
					(setq newStateList (cons newState stateList))
					(format t "~%Call #~a:" total)
					(describeState newState)
					(setq path (backTrack newStateList depthBound))
					(if (eq path 'Failed)
						'Failed
						(cons r path))))))

(print (backTrack (cons initialState 'NIL) 20))
