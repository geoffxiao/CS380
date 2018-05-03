# A -> B 0 
# A -> C 1
# B -> A 2
# B -> C 3
# C -> A 4
# C -> B 5
def applicableRules(state) :
	allowed = [0 for x in range(0, 6)]
	A = state[0]
	B = state[1]
	C = state[2]

	if B > A :
		allowed[0] = 0
		allowed[2] = 1

	if C > A :
		allowed[1] = 0
		allowed[4] = 1

	if C > B :
		allowed[3] = 0
		allowed[5] = 1

	if A == B :
		allowed[0] = 1

	if A == C :
		allowed[1] = 1
	
	if B == C :
		allowed[3] = 1

	return allowed

def applyRule(move, state) :
	out = state[:]

	# A -> B
	if move == 0 :
		out[0] = out[0] - out[1]
		out[1] = out[1] * 2

	# A -> C 
	if move == 1 :	
		out[0] = out[0] - out[2]
		out[2] = out[2] * 2

	# B -> A
	if move == 2 :
		out[1] = out[1] - out[0]
		out[0] = 2 * out[0]

	# B -> C
	if move == 3 :
		out[1] = out[1] - out[2]
		out[2] = 2 * out[2]

	# C -> A
	if move == 4 :
		out[2] = out[2] - out[0]
		out[0] = 2 * out[0]

	# C -> B
	if move == 5 :
		out[2] = out[2] - out[1]
		out[1] = 2 * out[1]

	# sort to leverage symmetry
	out.sort()
	return out

def deadEnd( state ) :
	if (state[0] + state[1] + state[2]) % 2 == 1 :
		return True
	
	if (state[0] + state[1] + state[2]) % 3 != 0 :
		return True

	if state[0] == 0 or state[1] == 0 or state[2] == 0 :
		return True
	
	return False

def goal( state ):
	return state[0] == state[1] and state[1] == state[2]

def backTrack( stateList, depthBound ) :
	state = stateList[0]
	
	if stateList[1 : ] != [] and state in stateList[1 : ] :
		return 1

	if deadEnd(state) :
		return 1

	if goal(state) :
		return []

	ruleSet = applicableRules(state)

	if ruleSet == [0, 0, 0, 0, 0, 0] :
		return 1
	
	for r in range(0, 6) :
		if ruleSet[r] == 1 :
			newState = applyRule(r, state)
			newStateList = stateList[:]
			newStateList.insert(0, newState)
			path = backTrack(newStateList, depthBound)

			# Not a failure, part of the goal state
			if path != 1 : 
				return [r] + path

	return 1 

initialState = [6, 7, 11]
initialState = [5, 6, 7]

initialState.sort()
solution = backTrack([initialState], 10)

newState = initialState
if solution == 1 :
	print(newState)
	print('Impossible')
else :
	for r in solution :
		print(newState)
		newState = applyRule(r, newState)
	print(newState)
