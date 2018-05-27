#-------------------------------------------------------------------------
# TacTical
# This program is designed to play Tac-Tical, using lookahead and board heuristics.
# It will allow the user to play a game against the machine, or allow the machine
# to play against itself for purposes of learning to improve its play.  All 'learning'
# code has been removed from this program.  
#
# Tac-Tical is a 2-player game played on a grid.  Each player has the same number 
# of tokens distributed on the grid in an initial configuration.  On each turn, a player 
# may move one of his/her tokens one unit either horizontally or vertically (not 
# diagonally) into an unoccupied square.  The object is to be the first player to get 
# three tokens in a row, either horizontally, vertically, or diagonally. 
#
# The board is represented by a matrix with extra rows and columns forming a 
# boundary to the playing grid.  Squares in the playing grid can be occupied by 
# either 'X', 'O', or 'Empty' spaces.  The extra elements are filled with 'Out of Bounds'
# squares, which makes some of the computations simpler.  
#-------------------------------------------------------------------------

from __future__ import print_function
import random
from random import randrange
import math

def GetMoves (Player, Board): 
#-------------------------------------------------------------------------
# Determines all legal moves for Player with current Board, 
# and returns them in MoveList.  
#-------------------------------------------------------------------------

	MoveList = []
	for i in range(1,NumRows+1):
		for j in range(1,NumCols+1):
			if Board[i][j] == Player:
			#-------------------------------------------------------------
			#  Check move directions (m,n) = (-1,0), (0,-1), (0,1), (1,0)  
			#-------------------------------------------------------------
				for m in range(-1,2):
					for n in range(-1,2):
						if abs(m) != abs(n):
							if Board[i + m][j + n] == Empty:
								MoveList.append([i, j, i+m, j+n])

	return MoveList


def GetHumanMove (Player, Board):  
#-------------------------------------------------------------------------
# If the opponent is a human, the user is prompted to input a legal move.  
# Determine the set of all legal moves, then check input move against it. 
#-------------------------------------------------------------------------
	MoveList = GetMoves(Player, Board)
	Move = None

	while(True):
		FromRow, FromCol, ToRow, ToCol = map(int, \
			input('Input your move (FromRow, FromCol, ToRow, ToCol): ').split(' '))

		ValidMove = False
		if not ValidMove:
			for move in MoveList:
				if move == [FromRow, FromCol, ToRow, ToCol]:
					ValidMove = True
					Move = move

		if ValidMove:
			break

		print('Invalid move.  ')

	return Move


def ApplyMove (Board, Move): 
#-------------------------------------------------------------------------
# Perform the given move, and update Board. 
#-------------------------------------------------------------------------

	FromRow, FromCol, ToRow, ToCol = Move
	Board[ToRow][ToCol] = Board[FromRow][FromCol]
	Board[FromRow][FromCol] = Empty
	return Board


def InitBoard (Board):  
#-------------------------------------------------------------------------
# Initialize the game board. 
#-------------------------------------------------------------------------

	for i in range(0,BoardRows+1):
		for j in range(0,BoardCols+1):
			Board[i][j] = OutOfBounds
 
	for i in range(1,NumRows+1):
		for j in range(1,NumCols+1):
			Board[i][j] = Empty

	for j in range(1,NumCols+1):
		if odd(j):
			Board[1][j] = x
			Board[NumRows][j] = o
		else:
			Board[1][j] = o
			Board[NumRows][j] = x
 

def odd(n):  
	return n%2==1

def ShowBoard (Board):  
	print("")
	row_divider = "+" + "-"*(NumCols*4-1) + "+"
	print(row_divider)

	for i in range(1,NumRows+1):
		for j in range(1,NumCols+1):
			if Board[i][j] == x:
				print('| X ',end="")
			elif Board[i][j] == o:
				print('| O ',end="")
			elif Board[i][j] == Empty:
				print('|   ',end="")
		print('|')
		print(row_divider)

	print("")


def Win (Player, Board):
#-------------------------------------------------------------------------
# Determines if Player has won, by finding '3 in a row'. 
#-------------------------------------------------------------------------

	loc = WherePlayer(Player, Board) # Where is the player?
	
	# Do 3 of the 4 points form a line?
	# 4 variations of a line
	#
	# 4 Points = 0 1 2 3
	# 0 1 2
	# 0 1 3
	# 1 2 3

	lines = [(0, 1, 2), (0, 1, 3), (1, 2, 3), (2, 3, 0)]

	for i in range(0, 4) :
		pt1 = lines[i][0]
		pt2 = lines[i][1]
		pt3 = lines[i][2]

		# a = row, b = col
		a = loc[pt1][0]
		b = loc[pt1][1]

		c = loc[pt2][0]
		d = loc[pt2][1]

		e = loc[pt3][0]
		f = loc[pt3][1]

		# horizontal row?
		if abs(a - c) == 1 and abs(c - e) == 1 and b - d == 0 and d - f == 0 :
			return True

		# vertical col?
		if abs(b - d) == 1 and abs(d - f) == 1 and a - c == 0 and c - e == 0 :
			return True

		# diagonal?
		if not (a - c == 0) : # avoid vertical lines, divide by zero err
			m = float(b - d) / float(a - c)
			
			y = m * (e - a) + f

			# does (e, f) line on the same line as (a, b) and (c, d)?
			if y == f :

				# Are they adjacent?
				if abs(a - c) == 1 and abs(c - e) == 1 and \
				abs(b - d) == 1 and abs(d - f) == 1 :
					return True
		

	# Not a win
	return False


# Find where Player is
def WherePlayer (Player, Board) :
	out = []
	for i in range(1, NumRows + 1) :
		for j in range(1, NumCols + 1) :
			if(Board[i][j] == Player) :
				out.append((i,j))

	return out



def GetComputerMove (Player, Board): 
#-------------------------------------------------------------------------
# If the opponent is a computer, use artificial intelligence to select
# the best move. 
# For this demo, a move is chosen at random from the list of legal moves.
#-------------------------------------------------------------------------
	MoveList = GetMoves(Player, Board)
	Move = AlphaBetaSearch(Player, Board)
	return Move


# Alpha Beta Search
def AlphaBetaSearch (Player, Board) :
	(best_score, best_move) = MaxValue(Player, Board, -infinity, infinity, 0)
	MoveList = GetMoves(Player, Board)
	print(best_score)
	return MoveList[best_move]
	

# MAX player
# Return best_score, best_move tuple
def MaxValue (Player, Board, alpha, beta, Depth) :

	# Terminal node? Only one move possible
	if TerminalTest(Board, Depth) :
		return (Utility(Player, Board), 0)

	i = 0 # move counter
	v = -infinity # best score
	best_move = 0 # best move

	for s in Successors(Player, Board) :
		new = MinValue(Player, s, alpha, beta, Depth + 1)
		new = new[0]

		# new best move and max v
		if new > v :
			best_move = i
			v = new

		if v >= beta :
			return (v,best_move)

		alpha = max(alpha, v)
		i = i + 1

	return (v,best_move)



# MIN player
def MinValue (Player, Board, alpha, beta, Depth) :
	
	if TerminalTest(Board, Depth) :
		return (Utility(Player, Board), 0)
	
	i = 0
	v = infinity  # best score
	best_move = 0 # best move

	for s in Successors(Player, Board) :
		new = MaxValue(Player, s, alpha, beta, Depth + 1)
		new = new[0]		

		# new best move and min v
		if new < v :
			best_move = i
			v = new

		if v <= alpha :
			return (v,best_move)

		beta = min(beta, v)
		i = i + 1
	
	return (v,best_move)


# Terminal state
# depth exceeded or win
def TerminalTest (Board, Depth) :
	if Depth >= MaxDepth :
		return True

	if Win(x, Board) or Win(o, Board) :
		return True

	return False



# Heuristic
def Utility (Player, Board) :
	if Win(Player, Board) :
		return infinity
		
	if Win(-Player, Board) :
		return -infinity

	# count num pieces adjacent
	value = hcoeff[0] * AdjPieces(Player, Board) - hcoeff[1] * AdjPieces(Player, Board) + \
	hcoeff[2] * DistToCenter(Player, Board) - hcoeff[3] * DistToCenter(-Player, Board) + \
	-hcoeff[4] * SumDistBtwPieces(Player, Board) + hcoeff[5] * SumDistBtwPieces(-Player, Board) + \
	-hcoeff[6] * Blocking(Player, Board) + hcoeff[7] * Blocking(-Player,Board) + \
	hcoeff[8] * len(GetMoves(Player,Board)) - hcoeff[9] * len(GetMoves(-Player,Board))

	# normalize
	if value > infinity or value < -infinity :
		value = (value - -infinity) / (2 * infinity)

	return value


# Two pieces form a circle, count how many of the opp pieces are within that
# circle
def Blocking (Player, Board) :
	loc = WherePlayer(Player, Board)	
	locOpp = WherePlayer(-Player, Board)

	out = 0
	for i in range(0, 4) :
		for j in range(0, 4) :
			if i != j :
				centerx = (loc[i][0] + loc[j][0]) * 0.5
				centery = (loc[i][1] + loc[j][1]) * 0.5
				r = DistBtwPts(loc[i], loc[j]) * 0.5				

				for k in range(0, 4) :
					if math.pow((locOpp[k][0] - centerx),2) + \
					math.pow((locOpp[k][1] - centery),2) <= math.pow(r,2) + 1e-2 :
						out = out + 1
	
	return out	



# Distances between all pairs of pieces
# double count the pairs
def DistancesBtwPairs (Player, Board) :
	loc = WherePlayer(Player, Board)

	out = []
	for i in range(0, 4) :
		for j in range(0, 4) :
			if i != j :
				out.append(DistBtwPts(loc[i], loc[j]))

	return out


# Sum of dist between all pieces
# divide by 2 b/c double counted
def SumDistBtwPieces (Player, Board) :
	return sum(DistancesBtwPairs(Player, Board)) / 2


# Count num pieces adjacent
def AdjPieces (Player, Board) :
	DstPairs = DistancesBtwPairs(Player, Board)
	out = 0

	for i in range(0, len(DstPairs)) :
		if DstPairs[i] < (math.sqrt(2) + 1e-3) :
			out = out + 1

	return out


def DistBtwPts (pt1, pt2) :
	return math.sqrt( math.pow((pt1[0] - pt2[0]),2) \
	+ math.pow((pt1[1] - pt2[1]),2) )	


# Count distance to center (3,3) (3,2)
def DistToCenter (Player, Board) :
	loc = WherePlayer(Player, Board)

	out = 0
	for i in range(0, 4) :
		out = out + max(DistBtwPts(loc[i], (3,3)), DistBtwPts(loc[i], (3,2)))

	return out



# Get successors
def Successors (Player, Board) :
	MoveList = GetMoves(Player, Board)
	StateList = []
	
	for m in MoveList :
		# Deep copy the board
		BoardTemp = [row[:] for row in Board]
		StateList.append(ApplyMove(BoardTemp, m))

	return StateList




def GetComputerMove2 (Player, Board):
	MoveList = GetMoves(Player, Board)
	Move = AlphaBetaSearch2(Player, Board)
	return Move

# Alpha Beta Search
def AlphaBetaSearch2 (Player, Board) :
	(best_score, best_move) = MaxValue2(Player, Board, -infinity, infinity, 0)
	MoveList = GetMoves(Player, Board)
	print(best_score)
	return MoveList[best_move]
	

# MAX player
# Return best_score, best_move tuple
def MaxValue2 (Player, Board, alpha, beta, Depth) :

	# Terminal node? Only one move possible
	if TerminalTest(Board, Depth) :
		return (Utility2(Player, Board), 0)

	i = 0 # move counter
	v = -infinity # best score
	best_move = 0 # best move

	for s in Successors(Player, Board) :
		new = MinValue2(Player, s, alpha, beta, Depth + 1)
		new = new[0]

		# new best move and max v
		if new > v :
			best_move = i
			v = new

		if v >= beta :
			return (v,best_move)

		alpha = max(alpha, v)
		i = i + 1

	return (v,best_move)



# MIN player
def MinValue2 (Player, Board, alpha, beta, Depth) :
	
	if TerminalTest(Board, Depth) :
		return (Utility2(Player, Board), 0)
	
	i = 0
	v = infinity  # best score
	best_move = 0 # best move

	for s in Successors(Player, Board) :
		new = MaxValue2(Player, s, alpha, beta, Depth + 1)
		new = new[0]		

		# new best move and min v
		if new < v :
			best_move = i
			v = new

		if v <= alpha :
			return (v,best_move)

		beta = min(beta, v)
		i = i + 1
	
	return (v,best_move)


# Heuristic
def Utility2 (Player, Board) :
	if Win(Player, Board) :
		return infinity
		
	if Win(-Player, Board) :
		return -infinity

	# count num pieces adjacent
	value = hcoeff2[0] * AdjPieces(Player, Board) - hcoeff2[1] * AdjPieces(Player, Board) + \
	hcoeff2[2] * DistToCenter(Player, Board) - hcoeff2[3] * DistToCenter(-Player, Board) + \
	-hcoeff2[4] * SumDistBtwPieces(Player, Board) + hcoeff2[5] * SumDistBtwPieces(-Player, Board) + \
	-hcoeff2[6] * Blocking(Player, Board) + hcoeff2[7] * Blocking(-Player,Board) + \
	hcoeff2[8] * len(GetMoves(Player,Board)) - hcoeff2[9] * len(GetMoves(-Player,Board))


	# normalize
	if value > infinity or value < -infinity :
		value = (value - -infinity) / (2 * infinity)

	return value




if __name__ == "__main__":
#-------------------------------------------------------------------------
# A move is represented by a list of 4 elements, representing 2 pairs of 
# coordinates, (FromRow, FromCol) and (ToRow, ToCol), which represent the 
# positions of the piece to be moved, before and after the move. 
#-------------------------------------------------------------------------
	x = -1
	o = 1
	Empty = 0
	OutOfBounds = 2
	NumRows = 5
	BoardRows = NumRows + 1 
	NumCols = 4
	BoardCols = NumCols + 1 
	MaxMoves = 4*NumCols 
	NumInPackedBoard = 4 * (BoardRows+1) *(BoardCols+1) 
	infinity = 10000  # Value of a winning board 
	MaxDepth = 4
#	Board = [[0 for col in range(BoardCols+1)] for row in range(BoardRows+1)]

	print("\nThe squares of the board are numbered by row and column, with '1 1' ")
	print("in the upper left corner, '1 2' directly to the right of '1 1', etc.")
	print("")
	print("Moves are of the form 'i j m n', where (i,j) is a square occupied")
	print("by your piece, and (m,n) is the square to which you move it.")
	print("")
	print("You move the 'X' pieces.\n")

#	InitBoard(Board) 
#	ShowBoard(Board) 

#	MoveList = GetMoves(x,Board)
#	print(MoveList)
#	MoveList = GetMoves(o,Board)
#	print(MoveList)

	# ---------------------------------------- #
	# Modified
	# ---------------------------------------- #
#	for n in range(5):

	Trials = 1
	Wins = [0 for i in range(Trials)]
	hcoeff = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
	hcoeff2 = [0.5, 0.5, 1, 2, 3, 0.4, 3, 2, 10, 2]

	for t in range(Trials) :
		Board = [[0 for col in range(BoardCols+1)] for row in range(BoardRows+1)]
		InitBoard(Board)	

		# randomize hcoeff2 slightly
		

		# Play game
		for n in range(100) :
	
			print("Player x\n")
			Move = GetComputerMove(x,Board)
			Board = ApplyMove(Board,Move)
			ShowBoard(Board) 
			if Win(x, Board) :
				print("Player x has won\n")
				Wins[t] = x
				break

			print("Player o\n")
			Move = GetComputerMove2(o,Board)
			Board = ApplyMove(Board,Move)
			ShowBoard(Board) 
			if Win(o, Board) :
				print("Player o has won\n")
				Wins[t] = o
				break

	print(Wins)


