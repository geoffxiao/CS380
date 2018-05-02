# Geoffrey Xiao
# CS 380
# 
#
# Python version
#	 Python 2.7.12 (default, Dec  4 2017, 14:50:18)
#	 [GCC 5.4.0 20160609] on linux2
# 
# Done on tux
#	 Linux version 4.4.0-116-generic (buildd@lgw01-amd64-021) (gcc version 5.4.0
#	 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.9) ) #140-Ubuntu SMP Mon Feb 12
#	 21:23:04 UTC 2018

import random # For randomly flailing
import math
import copy

# represent m x n board as a list
# m = 3, n = 4
#
# 11 10 9 8
#  7  6 5 4
#  3  2 1 0
#
# board = binary number
# ith digit = ith cell
# 	1 = peg is occupied
# 	0 = peg is not occupied
#
# rule = [jumper, goner, newpos] 
# jumper, goner, newpos = cell 0 to m * n - 1


# -----------------------------------------------------------
# Global Constants
finalPos = 9; # empty peg here, 9th peg is empty
m = 4; # number of rows
n = 4; # number of columns

# Create initial state for m x n board
initialState = ((2 ** (m * n)) - 1) & (~(2 ** finalPos))
# -----------------------------------------------------------


# Has the goal been reached?
def goal(state) :
	# only one peg remains and final peg is in 9th hole
	if state == 2 ** finalPos:
		return True

	else :
		return False

# Apply rule to a given state
# Return new state that is the result of the rule application
def applyRule(rule, state) :
	# Don't modify the state passed to it
	out = copy.copy(state)

	jumper = 2 ** rule[0]
	goner = 2 ** rule[1]
	newpos = 2 ** rule[2]

	# Create new state
	out = out - jumper - goner + newpos
	return out

# Get the row the cell is in
def row(cell) :
	return cell / n

# Get the column the cell is in
def col(cell) :
	return cell % n 

# Get the cell (0/1) in state
def access(state, cell) :
	cellNum = state >> cell # state = 01010111, cell = 3, cellNum = 01010
	return cellNum & 1 # 01010 & 00001 = 1	

# Are jumper, goner, newpos left/right of one another?
def precondLeftRight(rule, state) : 
	jumper = int(math.log(rule[0], 2))
	goner = int(math.log(rule[1], 2))
	newpos = int(math.log(rule[2], 2))

	# seperated by 1 = adjacent
	# on the same row
	# jumper, goner, newpos in the correct order
	if abs(jumper - goner) == 1 and abs(goner - newpos) == 1 and \
		row(jumper) == row(goner) and \
		row(goner) == row(newpos) and \
		jumper - goner == goner - newpos :	
		return True
	
	else :
		return False

# Are jumper, goner, newpos up/down of one another?
def precondUpDown(rule, state) : 
	jumper = int(math.log(rule[0], 2))
	goner = int(math.log(rule[1], 2))
	newpos = int(math.log(rule[2], 2))

	# in adjacent rows
	# in the same column
	# jumper, goner, newpos in the correct order
	if col(jumper) == col(goner) and col(goner) == col(newpos) and \
		abs(row(jumper) - row(goner)) == 1 and abs(row(goner) - row(newpos)) == 1 and \
		jumper - goner == goner - newpos :
		return True
	
	else :
		return False

# Are jumper, goner, newpos diagonal of one another?
def precondDiag(rule, state) :
	jumper = int(math.log(rule[0], 2))
	goner = int(math.log(rule[1], 2))
	newpos = int(math.log(rule[2], 2))

	# in adjacent columns
	# in adjacent rows
	# jumper, goner, newpos in the correct order
	if abs(col(jumper) - col(goner)) == 1 and \
		col(jumper) - col(goner) == col(goner) - col(newpos) and \
		abs(row(goner) - row(jumper)) == 1 and \
		row(jumper) - row(goner) == row(goner) - row(newpos) and \
		jumper - goner == goner - newpos :
		return True

	else :
		return False

# Is the rule allowed in state?
def precondition(rule, state) :
	jumper = int(math.log(rule[0], 2))
	goner = int(math.log(rule[1], 2))
	newpos = int(math.log(rule[2], 2))

	# jumper, goner, newpos left/right, up/down, diag oriented
	# jumper filled, goner filled, newpos empty
	if (precondLeftRight(rule, state) or precondUpDown(rule, state) or \
		precondDiag(rule, state)) and \
		access(state, jumper) == 1 and access(state, goner) == 1 and \
		access(state, newpos) == 0 :
		return True

	else :
		return False


# Find the applicable rules, the rules that satisfy the precondition
def applicableRules(state) :
	out = []

	# Loop through jumper
	for i in range(0, m * n) :
		# Loop through goner
		for j in range(0, m * n) :
			# Loop through newpos
			for k in range(0, m * n) :

				# Is the rule applicable?
				if precondition([2 ** i, 2 ** j, 2 ** k], state) :
					out.append([i, j, k])

	return out	


# Print the Peg Board
# State = [a0, a1, a2, ...]
def describeState(state) :
	counter = m * n - 1

	# Print the board from the top left hand corner a_m*n-1
	toPrint = ''

	for i in range(0, m) :
		for j in range(0, n) :
			cell = access(state, counter)
			if cell == 1 :
				print 'X',

			else :
				print 'O',

			counter = counter - 1

		print ''

# Explain the meaning of a rule
def describeRule(rule) :
	return 'Peg ' + str(rule[0]) + \
		' jumps over Peg ' + str(rule[1]) + \
		' and lands in Slot ' + str(rule[2])

# Flail wildly strategy
# Pick a random applicable rule
def flailWildly(state) :
	count = 0
	newState = state
	rules = applicableRules(state)

	print rules

	# Flail!
	while (not goal(newState)) and rules != [] :
		print 'State', count, ':'
		describeState(newState) # describe state
		print ''

		# Describe rules
		print 'Allowed Moves:'
		for i in range(0, len(rules)) :
			print describeRule(rules[i])
		print ''

		# Choose a random rule
		chosenRule = random.choice(rules)
		print 'Chosen Move:', describeRule(chosenRule)
		print '========================================'
	
		# Propagate state
		newState = applyRule(chosenRule, newState)

		rules = applicableRules(newState) # new rules
		count = count + 1

	# Success!
	if goal(newState) :
		print 'State', count, ':'
		describeState(newState)
		print ''
		print '========================================'
		print "Success!"	

	# Failure...
	elif rules  == [] :
		print 'State', count, ':'
		describeState(newState)
		print ''
		print '========================================'	
		print "Failure"


def backTrack(stateList, depthBound) :
	backTrack.total += 1
	s = stateList[0]
	ruleSet = applicableRules(s)

	if stateList[1 : ] != [] and s in stateList[1 : ] :
		backTrack.failures += 1
		return 'Failed-1' # Cycle

	# Dead-end
	# 'Failed-2'
	# No dead-end conditions here

	if goal(s) :
		return []

	if len(stateList) > depthBound :
		backTrack.failures += 1	
		return 'Failed-3' # Too deep

	if ruleSet == [] : # No Moves
		backTrack.failures += 1	
		return 'Failed-4'

	for r in ruleSet :
		newState = applyRule(r, s)
		newStateList = stateList[:]
		newStateList.insert(0, newState)
	
		print ''	
		print 'Total # of Calls: ', backTrack.total
		describeState(newState)
		print ''
		path = backTrack(newStateList, depthBound)

		# Not a failure, part of the goal state
		if( path != 'Failed-1' and path != 'Failed-2' and path != 'Failed-3' \
			 and path != 'Failed-4' and path != 'Failed-5' ) :
			return [r] + path
		
		else :
			print ''
			print path, ' at level ', len(stateList)
			print newStateList
			print ''

	backTrack.failures += 1
	return 'Failed-5'


# Flail wildly!
# flailWildly(initialState)

# Backtrack!
backTrack.total = 0
backTrack.failures = 0
backTrackSolution = backTrack([initialState], 50)
print 'Initial Board'
describeState(initialState)
print 'Backtrack solution:'
print backTrackSolution
print 'Total # Calls to backTrack: ', backTrack.total - 1
print 'Total # of Failures: ', backTrack.failures
