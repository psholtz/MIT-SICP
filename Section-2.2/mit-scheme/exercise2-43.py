#!/usr/bin/python

"""
Analyzing the performance of an algorithm is sometime easier for me to 
visualize in Python than in Lisp; hence why I hacked this out in Python.
"""

def accumulate(op,init,seq):
 if len(seq) == 0:
  return init
 else:
  return op(seq[0],accumulate(op,init,seq[1:]))

def append(a,b):
 return a+b

def flatmap(proc,seq):
 return accumulate(append,[],map(proc,seq))

def enumerate_interval(low,high):
 return range(low,high+1)

empty_board = []

def adjoin_position(new_row,column,rest_of_queens):
 """ hack needed to get it to work """
 a = [new_row]
 for x in rest_of_queens:
  a.append(x)
 return a

def safe(column,positions):
 def safe_iter(new_row,remaining_rows,row_offset):
  if len(remaining_rows) == 0:
   return True
  else:
   current_row = remaining_rows[0]
   if (new_row == current_row) or (new_row == current_row+row_offset) or (new_row == current_row-row_offset):
    return False
   else:
    return safe_iter(new_row,remaining_rows[1:],row_offset+1)
 return safe_iter(positions[0],positions[1:],1)

def queens(board_size):
 def queen_cols(k):
  if k==0:
   return [empty_board]
  else:
   return filter(lambda positions: safe(k,positions), flatmap(lambda rest_of_queens: map(lambda new_row: adjoin_position(new_row,k,rest_of_queens),enumerate_interval(1,board_size)),queen_cols(k-1)))
 return queen_cols(board_size)


#print flatmap(lambda rest_of_queens:map(lambda new_row:adjoin_position(new_row,1,rest_of_queens), enumerate_interval(1,4)), [[3, 1, 4]])

print queens(1)
print queens(2)
print queens(3)
print queens(4)
print len(queens(5))
print len(queens(6))
print len(queens(7))
print len(queens(8))
